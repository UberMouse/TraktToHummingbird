import api.Hummingbird.{HummingbirdShow, HummingbirdAnime}
import nz.ubermouse.hummingbirdsyncer.Main._

/**
 * Created by Taylor on 11/12/13.
 */
class MainTests extends UnitSpec {

  "unfoldRangeKeys" should "return a map with all keys in the format #-# unfolded into separate keys with the same value" in {
    val testMap = Map("1-3" -> "a", "4" -> "b")
    val expectedResult = Map("1" -> "a", "2" -> "a", "3" -> "a", "4" -> "b")
    
    unfoldRangeKeys(testMap) should be (expectedResult)
  }
}
