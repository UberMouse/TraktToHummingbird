import Main._
import org.scalatest.matchers.ClassicMatchers
import org.scalatest.FlatSpec
import Transformers._

/**
 * Created by Taylor on 11/12/13.
 */
class TransformerTests extends FlatSpec with ClassicMatchers {
  "highestEpisode" should "return Highest Episode in passed List[TraktActivity]" in {
    def traktActivityFactory(episodeNum:Int) = TraktActivity(null, TraktEpisode(episodeNum, 0))

    val testData = List(traktActivityFactory(1),
                        traktActivityFactory(2))

    assert(highestEpisode(testData) == traktActivityFactory(2))
  }

  "showRequiresSync" should "return true if library contains HummingbirdShow with slug matching TraktActivity.episode and HummingbirdShow.episodes_watched < TraktActivity.episode.episode" in {
    def hummingbirdShowFactory(episodesWatched:Int, slug:String) = HummingbirdShow(episodesWatched, HummingbirdAnime(null, slug))
    def traktActivityFactory(episode:Int, slug:String) = TraktActivity(TraktShow(null, -1, slug), TraktEpisode(episode, 0))

    val library = List(hummingbirdShowFactory(0, "show1"),
                       hummingbirdShowFactory(0, "show2"))

    assert(!showRequiresSync(traktActivityFactory(0, "show1"), library))
    assert(showRequiresSync (traktActivityFactory(1, "show1"), library))
    assert(!showRequiresSync(traktActivityFactory(0, "show3"), library))
  }

  {
    def traktActivityFactory(slug:String, tvdb_id:Int, episode:Int = 0, season:Int = 0) = TraktActivity(TraktShow(null, tvdb_id, slug), TraktEpisode(episode, season))
    def validMappingFactory(slug:String, seasonMap:Map[String, String] = null, specialMap:Map[String, String] = null) = ValidMapping(null, slug, seasonMap, specialMap)

    val getMapping = (x:Int) => {
      x match {
        case 0 => None
        case 1 => Some(validMappingFactory("show1"))
        case 2 => Some(validMappingFactory("show1", Map("1" -> "show2")))
        case 3 => Some(validMappingFactory("show1", null, Map("1" -> "show2")))
      }
    }

    "overrideShowNames" should "override TraktActivity slug with new slug if override exists for TraktActivity.tvdb_id or leave intact if none exist" in {
      assert(overrideShowNames(traktActivityFactory("show", 0), getMapping).show.slug == "show")
      assert(overrideShowNames(traktActivityFactory("show", 1), getMapping).show.slug == "show1")
    }

    "fixSeasons" should "override TraktActivity slug with new slug if season override exists for TraktActivity.tvdb_id or leave intact if none exist" in {
      assert(fixSeasons(traktActivityFactory("show1", 2, 0, 1), getMapping).show.slug == "show2")
      assert(fixSeasons(traktActivityFactory("show1", 2, 0, 0), getMapping).show.slug == "show1")
    }

    "fixSpecials" should "override TraktActivity slug with new slug if TraktActivity.show.season is 0 and special override exists for TraktActivity.show.episode or leave intact if none exist" in {
      assert(fixSpecials(traktActivityFactory("show1", 3, 1, 0), getMapping).show.slug == "show2")
      assert(fixSpecials(traktActivityFactory("show1", 3, 0, 0), getMapping).show.slug == "show1")
      assert(fixSpecials(traktActivityFactory("show1", 3, 0, 1), getMapping).show.slug == "show1")
    }
  }
}
