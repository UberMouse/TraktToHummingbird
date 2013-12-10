import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers
import Main._

/**
 * Created by Taylor on 11/12/13.
 */
class MainTests extends FlatSpec with ShouldMatchers {

  "unfoldRangeKeys" should "return a map with all keys in the format #-# unfolded into separate keys with the same value" in {
    val testMap = Map("1-2" -> "a", "3" -> "b")
    val expectedResult = Map("1" -> "a", "2" -> "a", "3" -> "b")
    
    unfoldRangeKeys(testMap) should be (expectedResult)
  }
}
