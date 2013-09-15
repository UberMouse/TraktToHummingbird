import scalaj.http._
import org.json4s.native.JsonMethods
import org.json4s.DefaultFormats
import org.streum.configrity._

object Main extends App {
  val HUMMINGBIRD_API = "https://hummingbirdv1.p.mashape.com"
  val TRAKT_API = "http://api.trakt.tv"

  case class HummingbirdConfig(authToken:String, mashapeAuth:String)

  case class TraktEpisode(episode:BigInt, season:BigInt)
  case class TraktShow(title:String)
  case class TraktActivity(show:TraktShow, episode:TraktEpisode)

  case class HummingbirdAnime(title:String, slug:String)
  case class HummingbirdShow(episodes_watched:BigInt, anime:HummingbirdAnime)

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
  while (true) {
    try {
      val library = retrieveHummingBirdLibrary(hummingbirdUsername)

      val highestEpisode = (x:(String, List[TraktActivity])) => {
        x._2.sortWith((x, y) => x.episode.episode > y.episode.episode).head
      }
      val showRequiresSync = (activity:TraktActivity) => {
        library.exists(x => x.anime.title.toLowerCase == activity.show.title.toLowerCase
                            && x.episodes_watched < activity.episode.episode)
      }
      val shows = getRecentTraktActivity(traktUsername,
                                         traktApiKey)
                                       .groupBy(_.show.title)
                                       .map(highestEpisode)
                                       .filter(showRequiresSync)

      shows.foreach(show => syncTraktToHummingbird(show, library))
      Thread.sleep(300000)
    }
    catch {
      case e: Exception => Thread.sleep(10000)
    }
  }

  def mkConnection(url:String, post:Boolean = false, mashapeAuth:String = "") = {
    val con = (if(post) Http.post(url) else Http(url)).option(HttpOptions.connTimeout(10000))
                                                      .option(HttpOptions.readTimeout(10000))
    if(mashapeAuth != "") con.header("X-Mashape-Authorization", mashapeAuth)
    else con
  }

  def syncTraktToHummingbird(traktActivity:TraktActivity,
                             hummingbirdLibrary:List[HummingbirdShow])(implicit config:HummingbirdConfig) {
    val hummingbirdShow = hummingbirdLibrary.find(x => x.anime.title.toLowerCase == traktActivity.show.title.toLowerCase).get
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
    if(con.asString.contains(slug))
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
    val con = mkConnection(s"$TRAKT_API/activity/user.json/$apiKey/$username/episode/scrobble")
    (JsonMethods.parse(con.asString) \ "activity").children.map(x => x.extract[TraktActivity])
  }
}