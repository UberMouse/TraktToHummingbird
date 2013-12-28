package nz.ubermouse.hummingbirdsyncer.api

import nz.ubermouse.hummingbirdsyncer.Main
import org.json4s.native.JsonMethods
import org.json4s.JsonAST.{JString, JField}
import org.json4s.DefaultFormats

/**
 * Created by Taylor on 28/12/13.
 */
object Sickbeard {
  implicit val formats = DefaultFormats

  val API_URL = "http://localhost:8081/api/123e959c1db648e25ff0660496993e9f"
  case class Result(message:String, result:String)

  def checkIfShowIsAdded(tvdbId:Int) = doCall(s"$API_URL/?cmd=show&tvdbid=$tvdbId")
  def addShowForDownload(tvdbId:Int) = doCall(s"$API_URL/?cmd=show.addnew&tvdbid=$tvdbId&status=wanted")

  private def doCall(url:String) = {
    val response = Main.mkConnection(url).asString
    val result = JsonMethods.parse(response).extract[Result]


    result.result == "success"
  }

}
