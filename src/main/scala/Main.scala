import java.util.Date
import org.json4s.JsonAST.{JField, JString}
import org.streum.configrity.converter.ValueConverter
import scala.collection.mutable
import scala.math.BigInt
import scalaj.http._
import org.json4s.native.JsonMethods
import org.json4s.DefaultFormats
import org.streum.configrity._
import Transformers._
import util.matching.Regex

object Main extends App {
  val HUMMINGBIRD_API = "https://hummingbirdv1.p.mashape.com"
  val TRAKT_API = "http://api.trakt.tv"
  val MAPPING_API = "http://localhost:50341/api"
  val ON_HOLD_STATUS = "on-hold"
  val CURRENTLY_WATCHING_STATUS = "currently-watching"

  case class HummingbirdConfig(authToken:String, mashapeAuth:String)

  case class TraktEpisode(episode:BigInt, season:BigInt)
  case class TraktShow(title:String, tvdb_id:Int, slug:String)
  case class TraktActivity(show:TraktShow, episode:TraktEpisode)

  case class HummingbirdAnime(title:String, slug:String)
  case class HummingbirdShow(episodes_watched:BigInt, anime:HummingbirdAnime, last_watched:Date, status:String)

  sealed abstract class HummingbirdMapping
  case class ValidMapping(TvDBId:String, OverrideSlug:String, SeasonOverrides:Map[String, String], SpecialOverrides:Map[String, String]) extends HummingbirdMapping
  case class EmptyMapping() extends HummingbirdMapping

  val defaults = Configuration("mashape-auth" -> "nZMJT9teIblQikXff081wAMuIDuFmkas",
                               "trakt-api-key" -> "fillmeout",
                               "trakt-username" -> "fillmeout",
                               "hummingbird-username" -> "fillmeout",
                               "hummingbird-email" -> "fillmeout",
                               "hummingbird-password" -> "fillmeout")

  val config = {
    try {
      Configuration.load("config.conf") include defaults
    } catch {
      case e:Exception =>
        defaults.save("config.conf")
        println("No config was found, created one with defaults. Please fill out config.conf")
        System.exit(-1)
        defaults
    }
  }

  SSLCert.disableCertificateValidation()

  val traktUsername = config[String]("trakt-username")
  val hummingbirdUsername = config[String]("hummingbird-username")
  val hummingbirdEmail = config[String]("hummingbird-email")
  val hummingbirdPassword = config[String]("hummingbird-password")
  val traktApiKey = config[String]("trakt-api-key")
  val mashapeAuth = config[String]("mashape-auth")

  implicit val formats = DefaultFormats
  implicit val hummingbirdConfig = HummingbirdConfig(getHummingbirdAuthToken(hummingbirdPassword,
                                                                             mashapeAuth,
                                                                             hummingbirdEmail,
                                                                             hummingbirdUsername),
                                                     mashapeAuth)

  val overrides = new mutable.HashMap[String, HummingbirdMapping]

  val json = mkConnection(s"$MAPPING_API/mapping").asString
  for(mapping <- JsonMethods.parse(json).children.map(x => x.extract[ValidMapping]))
    upsertMapping(mapping, load = true)

  while (true) {
    try {
      val currentlyWatching = retrieveHummingBirdLibrary(hummingbirdUsername)

      val shows = getRecentTraktActivity(traktUsername,
        traktApiKey)

      updateOverrides(shows)

      determineEpisodesToSync(shows, currentlyWatching).foreach(show => syncTraktShowToHummingbird(show, currentlyWatching))

      val onHold = retrieveHummingBirdLibrary(hummingbirdUsername, status = ON_HOLD_STATUS)
      determineShowsToUpdateStatus(currentlyWatching, onHold).foreach(show => {
        if (updateShowStatus(show._1, show._2)) {
          println(s"Changed ${show._1} to ${show._2}")
        }
      })

      println("Sync complete")

      Thread.sleep(300000)
    }
    catch {
      case e: Exception =>
        println(e.getMessage)
        Thread.sleep(10000)
    }
  }


  def determineShowsToUpdateStatus(currentlyWatching: List[HummingbirdShow], onHold: List[HummingbirdShow]) = {
    val curWatchingToUpdate = currentlyWatching.filter(x => (new Date().getTime - x.last_watched.getTime)
      >
      1000 * 60 * 60 * 24 * 7 * 2)
      .map(x => (x.anime.slug, ON_HOLD_STATUS))
    curWatchingToUpdate
  }

  def updateShowStatus(slug: String, status: String)(implicit config:HummingbirdConfig) = {
    val con = mkConnection(s"$HUMMINGBIRD_API/libraries/$slug",
                           post = true,
                           config.mashapeAuth).params("anime_id" -> slug,
                                                      "auth_token" -> config.authToken,
                                                      "status" -> status)
    val response = con.asString

    JsonMethods.parse(response).extractOpt[HummingbirdShow] exists (_.status == status)
  }

  def determineEpisodesToSync(shows: List[Main.TraktActivity],
                              library: List[Main.HummingbirdShow],
                              getMapping: Int => Option[ValidMapping] = (tvdb_id: Int) => getOverride(tvdb_id)) = {

    val reMappedShows = shows.groupBy(_.show.title)
                             .map(x => highestEpisode(x._2))
                             .map(x => overrideShowNames(x, getMapping))
                             .map(x => fixSeasons(x, getMapping))
                             .map(x => fixSpecials(x, getMapping))
                             
    reMappedShows.filter(x => showRequiresSync(x, library))
  }

  def updateOverrides(activities: List[TraktActivity]) {
    val needUpdate = activities.map(x => {
      getOverride(x.show.tvdb_id) match {
        case Some(_) => -1
        case None => x.show.tvdb_id
      }
    }).filter(_ > -1)

    if(needUpdate.length == 0) return

    val json = mkConnection(s"$MAPPING_API/mapping/bulk/${needUpdate.mkString(",")}").asString
    val parsedMappings = JsonMethods.parse(json).children.map(x => x.extract[ValidMapping])

    for(mapping <- parsedMappings)
      upsertMapping(mapping)

    for(id <- needUpdate) {
      getOverride(id) match {
        case Some(_) =>
        case None => overrides(id.toString) = EmptyMapping()
      }
    }
  }

  def getOverride(tvdbId:Int):Option[ValidMapping] = {
    overrides.get(tvdbId.toString) match {
      case Some(mapping) => mapping match {
        case m:EmptyMapping => None
        case m:ValidMapping => Some(m)
      }
      case None => None
    }
  }

  def unfoldRangeKeys(m:Map[String, String]) = m.flatMap{
    case (k,v) =>
      val validRange = "[0-9]+-[0-9]+".r

      k match {
        case validRange() =>
          val Array(l, r) = k split "-"
          val expanded = (l.toInt to r.toInt).map(x => (x.toString, v))
          Map(expanded:_*)

        case _ => Map(k -> v)
      }
    }

  def upsertMapping(mapping:ValidMapping, load:Boolean = false) {
    val unFolded = unfoldRangeKeys(mapping.SpecialOverrides)

    overrides(mapping.TvDBId) = mapping.copy(SpecialOverrides = unFolded)
    if(!load) println(s"Upserted mapping: $mapping")
  }

  def mkConnection(url:String, post:Boolean = false, mashapeAuth:String = "") = {
    val con = (if(post) Http.post(url) else Http(url)).option(HttpOptions.connTimeout(10000))
                                                      .option(HttpOptions.readTimeout(10000))
    if(mashapeAuth != "") con.header("X-Mashape-Authorization", mashapeAuth)
    else con
  }

  def syncTraktShowToHummingbird(traktActivity:TraktActivity,
                                 hummingbirdLibrary:List[HummingbirdShow])(implicit config:HummingbirdConfig) {
    val hummingbirdShow = hummingbirdLibrary.find(x => x.anime.slug.toLowerCase == traktActivity.show.slug.toLowerCase).get
    val slug = hummingbirdShow.anime.slug

    if(traktActivity.episode.episode - hummingbirdShow.episodes_watched < 0) return

    val updateParams = {
      if(traktActivity.episode.episode - hummingbirdShow.episodes_watched > 1)
        "episodes_watched" -> traktActivity.episode.episode.toString()
      else
        "increment_episodes" -> "true"
    }

    val con = mkConnection(s"$HUMMINGBIRD_API/libraries/$slug",
                           post = true,
                           config.mashapeAuth).params(updateParams, "auth_token" -> config.authToken)
    val response = con.asString
    if(response.contains(slug))
      println(s"Synced $slug to Hummingbird")
    else
      println(s"Failed to sync $slug to Hummingbird")
  }

  def retrieveHummingBirdLibrary(username:String, status:String = "currently-watching")(implicit config:HummingbirdConfig) = {
    val con = mkConnection(s"$HUMMINGBIRD_API/users/$username/library",
                           mashapeAuth = config.mashapeAuth).params("status" -> status,
                                                                    "auth_token" -> config.authToken)
    JsonMethods.parse(con.asString).children.map(x => x.extract[HummingbirdShow])
  }


  def getHummingbirdAuthToken(password:String, mashapeAuth:String, email:String = "", username:String = ""):String = {
    mkConnection(s"$HUMMINGBIRD_API/users/authenticate",
                 post = true,
                 mashapeAuth).params("password" -> password,
                                     if(email == "") "username" -> username else "email" -> email)
                              .asString.replaceAll("\"", "")
  }

  def getRecentTraktActivity(username:String, apiKey: String) = {
    val con = mkConnection(s"$TRAKT_API/activity/user.json/$apiKey/$username/episode/scrobble/${String.valueOf((System.currentTimeMillis()-10800000l)/1000l)}/${System.currentTimeMillis()/1000l}")
    (JsonMethods.parse(con.asString) \ "activity").transformField({
      case JField("url", JString(url)) => ("slug", JString(url.substring(url.lastIndexOf('/')+1)))
    }).children.map(x => x.extract[TraktActivity])
  }
}