import org.json4s.JsonAST._
import scalaj.http._
import org.json4s.native.JsonMethods
import org.json4s.DefaultFormats

object Main extends App {
  val HUMMINGBIRD_API = "https://hummingbirdv1.p.mashape.com"
  val TRAKT_API = "http://api.trakt.tv"

  case class HummingbirdConfig(authToken:String, mashapeAuth:String)

  case class TraktEpisode(episode:BigInt, season:BigInt)
  case class TraktShow(title:String)
  case class TraktActivity(show:TraktShow, episode:TraktEpisode)

  case class HummingbirdAnime(title:String, slug:String)
  case class HummingbirdShow(episodes_watched:BigInt, anime:HummingbirdAnime)

  val traktUsername = ""
  val hummingbirdUsername = ""
  val hummingbirdEmail = ""
  val hummingbirdPassword = ""
  val traktApiKey = ""
  val mashapeAuth =  ""

  implicit val formats = DefaultFormats
  implicit val hummingbirdConfig = HummingbirdConfig(getHummingbirdAuthToken(hummingbirdPassword,
                                                                             mashapeAuth,
                                                                             email = hummingbirdEmail),
                                                     mashapeAuth)
  val library = retrieveHummingBirdLibrary(hummingbirdUsername)

  val shows = getRecentTraktActivity(traktUsername,
                                     traktApiKey)
                                   .groupBy(_.show.title)
                                   .map(x => x._2.sortWith((x, y) => x.episode.episode > y.episode.episode).head)
                                   .filter(activity => {
    library.exists(x => x.anime.title == activity.show.title && x.episodes_watched < activity.episode.episode)
  })

  shows.foreach(show => syncTraktToHummingbird(show, library))

  def mkConnection(url:String, post:Boolean = false, mashapeAuth:String = "") = {
    val con = (if(post) Http.post(url) else Http(url)).option(HttpOptions.connTimeout(10000))
                                                      .option(HttpOptions.readTimeout(10000))
    if(mashapeAuth != "") con.header("X-Mashape-Authorization", mashapeAuth)
    else con
  }

  def syncTraktToHummingbird(traktActivity:TraktActivity,
                             hummingbirdLibrary:List[HummingbirdShow])(implicit config:HummingbirdConfig) {
    val hummingbirdShow = hummingbirdLibrary.filter(x => x.anime.title == traktActivity.show.title).head
    val slug = hummingbirdShow.anime.slug
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