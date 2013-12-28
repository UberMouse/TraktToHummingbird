package api

import java.util.Date
import org.json4s.native.JsonMethods
import scala.math.BigInt
import scalaj.http.{HttpOptions, Http}
import org.json4s.DefaultFormats
import nz.ubermouse.hummingbirdsyncer.Main
import Main.RichHttpRequest

/**
 * Created by Taylor on 28/12/13.
 */
object Hummingbird {
  implicit val formats = DefaultFormats

  val API_URL = "https://hummingbirdv1.p.mashape.com"
  case class HummingbirdConfig(authToken:String, mashapeAuth:String)

  case class HummingbirdAnime(title:String, slug:String)
  case class HummingbirdShow(episodes_watched:BigInt, anime:HummingbirdAnime, last_watched:Date, status:String)

  sealed abstract class HummingbirdMapping
  case class ValidMapping(TvDBId:String, OverrideSlug:String, SeasonOverrides:Map[String, String], SpecialOverrides:Map[String, String]) extends HummingbirdMapping
  case class EmptyMapping() extends HummingbirdMapping

  def updateShowStatus(slug: String, status: String)(implicit config:HummingbirdConfig) = {
    val con = createApiConnection(s"$API_URL/libraries/$slug",
                                post = true).addParams("anime_id" -> slug,
                                                    "status" -> status)
    val response = con.asString

    JsonMethods.parse(response).extractOpt[HummingbirdShow] exists (_.status == status)
  }

  def retrieveLibrary(username:String, status:String = "currently-watching")(implicit config:HummingbirdConfig) = {
    val con = createApiConnection(s"$API_URL/users/$username/library").params("status" -> status)
    JsonMethods.parse(con.asString).children.map(x => x.extract[HummingbirdShow])
  }


  def getAuthToken(password:String, mashapeAuth:String, email:String = "", username:String = ""):String = {
    Main.mkConnection(s"$API_URL/users/authenticate",
                      post = true).params("password" -> password,
                                          if(email == "") "username" -> username else "email" -> email)
                                  .header("X-Mashape-Authorization", mashapeAuth)
                      .asString.replaceAll("\"", "")
  }

  def updateShow(slug:String, params:(String, String)*)(implicit config:HummingbirdConfig) = {
    val con = createApiConnection(s"$API_URL/libraries/$slug",
                                post = true).addParams(params:_*)

    val response = con.asString
    if(response.contains(slug))
      true
    else
      false
  }

  def createApiConnection(url:String, post:Boolean = false)(implicit config:HummingbirdConfig) = {
    val con = (if(post) Http.post(url) else Http(url)).option(HttpOptions.connTimeout(10000))
                                                      .option(HttpOptions.readTimeout(10000))
                                                      .header("X-Mashape-Authorization", config.mashapeAuth)
                                                      .params("auth_token" -> config.authToken)
    con
  }

}
