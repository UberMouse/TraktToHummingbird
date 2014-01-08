package nz.ubermouse.hummingbirdsyncer

import _root_.api.Hummingbird
import _root_.api.Hummingbird._
import _root_.api.Hummingbird.HummingbirdConfig
import _root_.api.Hummingbird.HummingbirdShow
import _root_.api.Hummingbird.ValidMapping
import java.util.Date
import org.json4s.JsonAST.JField
import scala.collection.mutable
import scalaj.http._
import org.json4s.native.JsonMethods
import org.json4s.DefaultFormats
import org.streum.configrity._
import Transformers._
import scala.Some
import org.json4s.JsonAST.JString
import nz.ubermouse.hummingbirdsyncer.api.{Trakt, Sickbeard}
import nz.ubermouse.hummingbirdsyncer.api.Trakt.TraktActivity
import com.typesafe.scalalogging.slf4j.Logging

object Main extends App with Logging {
  val TRAKT_API = "http://api.trakt.tv"
  val MAPPING_API = "http://localhost:50341/api"
  val ON_HOLD_STATUS = "on-hold"
  val CURRENTLY_WATCHING_STATUS = "currently-watching"
  val LONG_SLEEP = 300000
  val SHORT_SLEEP = 10000

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

  val traktUsername = config[String]("trakt-username")
  val hummingbirdUsername = config[String]("hummingbird-username")
  val hummingbirdEmail = config[String]("hummingbird-email")
  val hummingbirdPassword = config[String]("hummingbird-password")
  val traktApiKey = config[String]("trakt-api-key")
  val mashapeAuth = config[String]("mashape-auth")

  implicit val formats = DefaultFormats
  implicit val hummingbirdConfig = HummingbirdConfig(Hummingbird.getAuthToken(hummingbirdPassword,
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
      val currentlyWatching = Hummingbird.retrieveLibrary(hummingbirdUsername)
      val shows = getRecentTraktActivity(traktUsername,
                                         traktApiKey)
      updateOverrides(shows)
      
      val remappedEpisodes = remapEpisodes(shows, currentlyWatching).toList
      val needSync = remappedEpisodes.filter(x => showRequiresSync(x, currentlyWatching))

      needSync.foreach(show => syncTraktShowToHummingbird(show, currentlyWatching))

      val onHold = Hummingbird.retrieveLibrary(hummingbirdUsername, status = ON_HOLD_STATUS)
      val statusUpdateNeeded = determineShowsToUpdateStatus(currentlyWatching, onHold, remappedEpisodes)
      statusUpdateNeeded.foreach(show => {
        if (updateShowStatus(show._1, show._2)) {
          log(s"Changed ${show._1} to ${show._2}")
        }
      })

      addNewOnHoldToSickbeard(onHold)

      println(s"Sync complete. Synced ${needSync.length} shows")

      if(!needSync.isEmpty) logger.debug(s"Sync complete. Synced ${needSync.map(x => x.show.title).mkString("\n")}")

      if(statusUpdateNeeded.length == 0)
        Thread.sleep(LONG_SLEEP)
      else
        Thread.sleep(SHORT_SLEEP/10)
    }
    catch {
      case e: Exception =>
        log(e.getMessage)
        Thread.sleep(SHORT_SLEEP)
    }
  }

  def addNewOnHoldToSickbeard(shows: List[HummingbirdShow]) = {
    val tvdbIds = shows.map(x => Trakt.getIdForShow(x.anime.title))
    for{id <- tvdbIds
        if !Sickbeard.checkIfShowIsAdded(id) && id != -1} {
      if(Sickbeard.addShowForDownload(id))
        log(s"Added $id to Sickbeard")
      else
        log(s"Failed adding $id to Sickbeard")
    }
  }


  def determineShowsToUpdateStatus(currentlyWatching: List[HummingbirdShow],
                                   onHold: List[HummingbirdShow],
                                   traktActivity:List[TraktActivity]) = {

    val curWatchingToUpdate = currentlyWatching.filter(x => (new Date().getTime - x.last_watched.getTime)
                                                            >
                                                            1000 * 60 * 60 * 24 * 7 * 2
                                                            && x.episodes_watched > 0)
                                               .map(x => (x.anime.slug, ON_HOLD_STATUS))
    val onHoldToUpdate = onHold.filter(x => traktActivity.exists(_.show.slug == x.anime.slug))
                               .map(x => (x.anime.slug, CURRENTLY_WATCHING_STATUS))
    curWatchingToUpdate ++ onHoldToUpdate
  }

  def remapEpisodes(shows: List[TraktActivity],
                    library: List[HummingbirdShow],
                    getMapping: Int => Option[ValidMapping] = (tvdb_id: Int) => getOverride(tvdb_id)) = {

    shows.groupBy(_.show.title)
         .map(x => highestEpisode(x._2))
         .map(x => overrideShowNames(x, getMapping))
         .map(x => fixSeasons(x, getMapping))
         .map(x => fixSpecials(x, getMapping))
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
    if(!load) log(s"Upserted mapping: $mapping")
  }

  def mkConnection(url:String, post:Boolean = false) = {
    val con = (if(post) Http.post(url) else Http(url)).option(HttpOptions.connTimeout(10000))
                                                      .option(HttpOptions.readTimeout(10000))
    con
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

    if(Hummingbird.updateShow(slug, updateParams)(config))
      logger.debug(s"Synced $slug to Hummingbird")
    else
      log(s"Failed to sync $slug to Hummingbird")
  }

  def getRecentTraktActivity(username:String, apiKey: String) = {
    val con = mkConnection(s"$TRAKT_API/activity/user.json/$apiKey/$username/episode/scrobble/${String.valueOf((System.currentTimeMillis()-10800000l)/1000l)}/${System.currentTimeMillis()/1000l}")

    (JsonMethods.parse(con.asString) \ "activity").transformField({
      case JField("url", JString(url)) => ("slug", JString(url.substring(url.lastIndexOf('/')+1)))
    }).children.map(x => x.extract[TraktActivity])
  }

  def log(msg:String) {
    println(msg)
    logger.debug(msg)
  }

  implicit class RichHttpRequest(req: Http.Request) {
    def addParams(params:(String, String)*) = {
      req.copy(params = req.params ++ params)
    }
  }
}