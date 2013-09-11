import org.json4s.JsonAST._
import scalaj.http._
import org.json4s.native.JsonMethods

object Main extends App {
  val HUMMINGBIRD_API = "https://hummingbirdv1.p.mashape.com"
  val TRAKT_API = "http://api.trakt.tv"
  val authToken = getHummingbirdAuthToken("", email = "")

  val library = retrieveHummingBirdLibrary("UberMouse", authToken)

  val shows = retrieveTraktShowData.groupBy(_._1)
                                   .map(x => x._2.sortWith((x, y) => x._2 > y._2).head)
                                   .filter(show => library.exists(x => x._1 == show._1 && x._2 < show._2))

  shows.foreach(show => syncTraktToHummingbird(show, library))

  def syncTraktToHummingbird(traktShow:(String, BigInt, BigInt),
                             hummingbirdLibrary:List[(String, BigInt, String)]) {
    val hummingbirdShow = hummingbirdLibrary.filter(x => x._1 == traktShow._1).head
    val slug = hummingbirdShow._3
    val updateParams = if(traktShow._2 - hummingbirdShow._2 > 1) "episodes_watched" -> traktShow._2.toString() else "increment_episodes" -> "true"
    val con = Http.post(s"$HUMMINGBIRD_API/libraries/$slug").params(updateParams,
                                                                    "auth_token" -> authToken)
                                                            .option(HttpOptions.connTimeout(10000))
                                                            .option(HttpOptions.readTimeout(10000))
                                                            .header("X-Mashape-Authorization", "")
    if(con.asString.contains(slug))
      println(s"Synced $slug to Hummingbird")
    else
      println(s"Failed to sync $slug to Hummingbird")
  }

  def retrieveHummingBirdLibrary(username:String, authToken:String):List[(String, BigInt, String)] = {
    val con = Http(s"$HUMMINGBIRD_API/users/$username/library").params("status" -> "currently-watching",
                                                                       "auth_token" -> authToken)
                                                               .option(HttpOptions.connTimeout(10000))
                                                               .option(HttpOptions.readTimeout(10000))
                                                               .header("X-Mashape-Authorization", "")

    for {
      JArray(shows) <- JsonMethods.parse(con.asString)
      JObject(showData) <- shows
      JField("episodes_watched", JInt(watched)) <- showData
      JField("anime", JObject(anime)) <- showData
      JField("title", JString(title)) <- anime
      JField("slug", JString(slug)) <- anime
    } yield (title, watched, slug)
  }


  def getHummingbirdAuthToken(password:String, email:String = "", username:String = "") = {
    Http.post(s"$HUMMINGBIRD_API/users/authenticate")
        .params("email" -> (if(email == "") username else email),
                "password" -> password)
        .option(HttpOptions.connTimeout(10000))
        .option(HttpOptions.readTimeout(10000))
        .header("X-Mashape-Authorization", "")
        .asString
        .replaceAll("\"", "")
  }

  def retrieveTraktShowData: List[(String, BigInt, BigInt)] = {
    for {
      JObject(showData) <- getRecentTraktShows("UberMouse", "")
      JField("show", JObject(show)) <- showData
      JField("episode", JObject(episode)) <- showData
      JField("title", JString(title)) <- show
      JField("season", JInt(season)) <- episode
      JField("episode", JInt(episode)) <- episode
    } yield (title, episode, season)
  }

  def getRecentTraktShows(username:String, apiKey: String) = {
    val con = Http(s"$TRAKT_API/activity/user.json/$apiKey/$username/episode/scrobble")
              .option(HttpOptions.connTimeout(10000))
              .option(HttpOptions.readTimeout(10000))
    JsonMethods.parse(con.asString)
  }
}
