package nz.ubermouse.hummingbirdsyncer.api

import nz.ubermouse.hummingbirdsyncer.Main
import org.json4s.native.JsonMethods
import scala.math.BigInt
import scala.BigInt
import org.json4s._
import java.net.URLEncoder

/**
 * Created by Taylor on 28/12/13.
 */
object Trakt {
  implicit val formats = DefaultFormats

  case class TraktEpisode(episode:BigInt, season:BigInt)
  case class TraktShow(title:String, tvdb_id:Int, slug:String)
  case class TraktActivity(show:TraktShow, episode:TraktEpisode)


  def getIdForShow(show:String) = {
    val response = Main.mkConnection(s"http://api.trakt.tv/search/shows.json/032627b12168fc80224b7bea1b087d78?query=${URLEncoder.encode(show, "UTF-8")}").asString
    val json = JsonMethods.parse(response)

    (json.children.head \ "tvdb_id").extract[Int]
  }
}
