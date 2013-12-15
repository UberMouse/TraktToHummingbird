import Main._
import org.scalatest.matchers.{ShouldMatchers, ClassicMatchers}
import org.scalatest.FlatSpec
import Transformers._

/**
 * Created by Taylor on 11/12/13.
 */
class TransformerTests extends UnitSpec {
  "highestEpisode" should "return Highest Episode in passed List[TraktActivity]" in {
    def traktActivityFactory(episodeNum:Int) = TraktActivity(null, TraktEpisode(episodeNum, 0))

    val testData = List(traktActivityFactory(1),
                        traktActivityFactory(2))

    highestEpisode(testData) should be (traktActivityFactory(2))
  }

  "showRequiresSync" should "return true if library contains HummingbirdShow with slug matching TraktActivity.episode and HummingbirdShow.episodes_watched < TraktActivity.episode.episode" in {
    def hummingbirdShowFactory(episodesWatched:Int, slug:String) = HummingbirdShow(episodesWatched, HummingbirdAnime(null, slug))
    def traktActivityFactory(episode:Int, slug:String) = TraktActivity(TraktShow(null, -1, slug), TraktEpisode(episode, 0))

    val library = List(hummingbirdShowFactory(0, "show1"),
                       hummingbirdShowFactory(0, "show2"))

    showRequiresSync(traktActivityFactory(0, "show1"), library) should be (false)
    showRequiresSync(traktActivityFactory(1, "show1"), library) should be (true)
    showRequiresSync(traktActivityFactory(0, "show3"), library) should be (false)
  }

  "overrideShowNames" should "override TraktActivity slug with new slug if override exists for TraktActivity.tvdb_id or leave intact if none exist" in {
    overrideShowNames(traktActivityFactory("show", 0), getMapping).show.slug should be ("show")
    overrideShowNames(traktActivityFactory("show", 1), getMapping).show.slug should be ("show1")
  }

  "fixSeasons" should "override TraktActivity slug with new slug if season override exists for TraktActivity.tvdb_id or leave intact if none exist" in {
    fixSeasons(traktActivityFactory("show1", 2, 0, 1), getMapping).show.slug should be ("show2")
    fixSeasons(traktActivityFactory("show1", 2, 0, 0), getMapping).show.slug should be ("show1")
  }

  "fixSpecials" should "override TraktActivity slug with new slug if TraktActivity.show.season is 0 and special override exists for TraktActivity.show.episode or leave intact if none exist" in {
    fixSpecials(traktActivityFactory("show1", 3, 1, 0), getMapping).show.slug should be("show3")
    fixSpecials(traktActivityFactory("show1", 3, 2, 0), getMapping).show.slug should be("show4")
    fixSpecials(traktActivityFactory("show1", 3, 0, 0), getMapping).show.slug should be("show1")
    fixSpecials(traktActivityFactory("show1", 3, 0, 1), getMapping).show.slug should be("show1")
  }
}
