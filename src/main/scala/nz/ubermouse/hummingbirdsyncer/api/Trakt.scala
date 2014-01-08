package nz.ubermouse.hummingbirdsyncer.api

import nz.ubermouse.hummingbirdsyncer.Main
import org.json4s.native.JsonMethods
import scala.math.BigInt
import scala.BigInt
import org.json4s._
import java.net.URLEncoder
import com.typesafe.scalalogging.slf4j.Logging

/**
 * Created by Taylor on 28/12/13.
 */
object Trakt extends Logging {
  implicit val formats = DefaultFormats

  case class TraktEpisode(episode:BigInt, season:BigInt)
  case class TraktShow(title:String, tvdb_id:Int, slug:String)
  case class TraktActivity(show:TraktShow, episode:TraktEpisode)


  def getIdForShow(show:String) = {
    val response = Main.mkConnection(s"http://api.trakt.tv/search/shows.json/032627b12168fc80224b7bea1b087d78?query=${URLEncoder.encode(show, "UTF-8")}").asString
    val json = JsonMethods.parse(response)

    if(json.children.isEmpty) {
      logger.debug(s"No results on Trakt for $show")
      -1
    }
    else (json.children.head \ "tvdb_id").extract[Int]
  }
}
