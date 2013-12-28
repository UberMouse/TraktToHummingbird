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

  "determineEpisodesToSync" should "return an Iterable containing supplied TraktActivitys that exist within the library, require syncing and have had slugs overriden" in {
    def hummingbirdShowFactory(episodesWatched:Int, slug:String) = HummingbirdShow(episodesWatched, HummingbirdAnime(null, slug), null, null)

    val library = List(hummingbirdShowFactory(1, "show1"),
                       hummingbirdShowFactory(1, "show2"),
                       hummingbirdShowFactory(0, "show3"))

    val shows = List(traktActivityFactory("show ", 1, 0, 1), //Episode that needs slug override and has less episodes watched
                     traktActivityFactory("show ", 1, 1, 1), //Episode that needs slug override and has the same episodes watched
                     traktActivityFactory("show ", 1, 2, 1), //Episode that needs slug override and has more episodes watched
                     traktActivityFactory("show2", 0, 0, 1), //Episode that has less episodes watched
                     traktActivityFactory("show2", 0, 1, 1), //Episode that has the same episodes watched
                     traktActivityFactory("show2", 0, 2, 1), //Episode that has more episodes watched
                     traktActivityFactory("show ", 3, 1, 0), //Special that needs slug override
                     traktActivityFactory("show5", 3, 3, 0)) //Special with range that needs slug override

    val expectedResult = List(traktActivityFactory("show1", 1, 2, 1),
                              traktActivityFactory("show2", 0, 2, 1),
                              traktActivityFactory("show3", 3, 3, 0))

    remapEpisodes(shows, library, getMapping) should be (expectedResult)
  }
}
