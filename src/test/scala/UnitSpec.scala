import api.Hummingbird.ValidMapping
import nz.ubermouse.hummingbirdsyncer.api.Trakt.{TraktActivity, TraktShow, TraktEpisode}
import nz.ubermouse.hummingbirdsyncer.Main._
import org.scalatest.{Matchers, FlatSpec}
import org.scalatest.matchers.ShouldMatchers

/**
 * Created by Taylor on 11/12/13.
 */
class UnitSpec extends FlatSpec with Matchers {
  def traktActivityFactory(slug:String, tvdb_id:Int, episode:Int = 0, season:Int = 0) = TraktActivity(TraktShow(null, tvdb_id, slug), TraktEpisode(episode, season))
  def validMappingFactory(slug:String, seasonMap:Map[String, String] = null, specialMap:Map[String, String] = null) = ValidMapping(null, slug, seasonMap, specialMap)

  val getMapping = (x:Int) => x match {
      case 0 => None
      case 1 => Some(validMappingFactory("show1"))
      case 2 => Some(validMappingFactory("show1", Map("1" -> "show2")))
      case 3 => Some(validMappingFactory("show1", Map(), unfoldRangeKeys(Map("1" -> "show3", "2-3" -> "show4"))))
      case 20 => Some(validMappingFactory("show3", Map("2" -> "+58")))
  }
}
