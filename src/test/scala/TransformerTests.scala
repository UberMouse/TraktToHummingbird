import api.Hummingbird.{HummingbirdAnime, HummingbirdShow}
import nz.ubermouse.hummingbirdsyncer.api.Trakt.{TraktShow, TraktActivity, TraktEpisode}
import nz.ubermouse.hummingbirdsyncer.Transformers._

/**
 * Created by Taylor on 11/12/13.
 */
class TransformerTests extends UnitSpec {

  {
    def traktActivityFactory(episode:Int, season:Int = 0) = TraktActivity(null, TraktEpisode(episode, season))

    "highestEpisode" should "return Highest Episode in passed List[TraktActivity]" in {
      val highest = traktActivityFactory(3)
      val testData = List(traktActivityFactory(1),
                          highest,
                          traktActivityFactory(2))

      highestEpisode(testData) should be (highest)
    }

    "highestEpisode" should "only consider largest season" in {
      val highest = traktActivityFactory(3, 2)
      val testData = List(traktActivityFactory(5), traktActivityFactory(1, 2), highest)

      highestEpisode(testData) should be (highest)
    }
  }

  "showRequiresSync" should "return true if library contains HummingbirdShow with slug matching TraktActivity.episode and HummingbirdShow.episodes_watched < TraktActivity.episode.episode" in {
    def hummingbirdShowFactory(episodesWatched:Int, slug:String) = HummingbirdShow(episodesWatched, HummingbirdAnime(null, slug), null, null)
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
