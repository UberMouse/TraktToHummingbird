import org.json4s.JsonAST.{JField, JString}
import org.streum.configrity.converter.ValueConverter
import scala.collection.mutable
import scalaj.http._
import org.json4s.native.JsonMethods
import org.json4s.DefaultFormats
import org.streum.configrity._

object Main extends App {
  implicit val TUPLE_CONVERTER1 = TupleConverter1
  implicit val TUPLE_CONVERTER2 = TupleConverter2
  val HUMMINGBIRD_API = "https://hummingbirdv1.p.mashape.com"
  val TRAKT_API = "http://api.trakt.tv"
  val MAPPING_API = "http://localhost:50341/api"

  case class HummingbirdConfig(authToken:String, mashapeAuth:String)

  case class TraktEpisode(episode:BigInt, season:BigInt)
  case class TraktShow(title:String, tvdb_id:Int, slug:String)
  case class TraktActivity(show:TraktShow, episode:TraktEpisode)

  case class HummingbirdAnime(title:String, slug:String)
  case class HummingbirdShow(episodes_watched:BigInt, anime:HummingbirdAnime)

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
      case e:Exception => {
        defaults.save("config.conf")
        println("No config was found, created one with defaults. Please fill out config.conf")
        System.exit(-1)
        defaults
      }
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
    overrides(mapping.TvDBId) = mapping

  while (true) {
    try {
      val library = retrieveHummingBirdLibrary(hummingbirdUsername)

      val highestEpisode = (x:(String, List[TraktActivity])) => {
        x._2.sortWith((x, y) => x.episode.episode > y.episode.episode).head
      }
      val showRequiresSync = (x:TraktActivity) => {
        library.exists(y => y.anime.slug.toLowerCase == x.show.slug.toLowerCase
                            && y.episodes_watched < x.episode.episode)
      }
      val overrideShowNames = (x:TraktActivity) => {
        x.copy(x.show.copy(slug = getOverride(x.show.tvdb_id).map(x => x.OverrideSlug).getOrElse(x.show.slug)))
      }

      val fixSeasons = (x:TraktActivity) => {
        getOverride(x.show.tvdb_id) match {
          case Some(show) => {
            val seasons = show.SeasonOverrides
            val zip = seasons.keys.zip(seasons.values)
            val option = seasons.get(x.episode.season.toString())
            option match {
              case Some(slug) => x.copy(x.show.copy(slug = slug))
              case None => x
            }
          }
          case None => x
        }
      }

      val shows = getRecentTraktActivity(traktUsername,
                                         traktApiKey)

      updateOverrides(shows)

      val reMappedShows = shows.groupBy(_.show.title)
                               .map(highestEpisode)
                               .map(overrideShowNames)
                               .map(fixSeasons)

      reMappedShows.filter(showRequiresSync)
                   .foreach(show => syncTraktShowToHummingbird(show, library))

      println("Synced")

      Thread.sleep(300000)
    }
    catch {
      case e: Exception => {
        e.printStackTrace()
        Thread.sleep(10000)
      }
    }
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
      overrides(mapping.TvDBId) = mapping

    for(id <- needUpdate) {
      getOverride(id) match {
        case Some(_) => {}
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
    val string = con.asString
    println(string)
    if(string.contains(slug))
      println(s"Synced $slug to Hummingbird")
    else
      println(s"Failed to sync $slug to Hummingbird")
  }

  def retrieveHummingBirdLibrary(username:String)(implicit config:HummingbirdConfig) = {
    val con = mkConnection(s"$HUMMINGBIRD_API/users/$username/library",
                           mashapeAuth = config.mashapeAuth).params("status" -> "currently-watching",
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

  object TupleConverter1 extends ValueConverter[(Int, String)] {
    def parse( s: String ) = {
      val split = s.split(",").map(x => x.replaceAll("[\\(\\)]", ""))
      (Integer.parseInt(split(0)), split(1))
    }
  }

  object TupleConverter2 extends ValueConverter[(String, String)] {
    def parse( s: String ) = {
      val split = s.split(",").map(x => x.replaceAll("[\\(\\)]", ""))
      (split(0), split(1))
    }
  }
}