import nz.ubermouse.hummingbird.tvdb.{TvdbSearchResult, TvdbSearcher}
import org.scalatest._

class TvdbSpec extends FeatureSpec with GivenWhenThen with Matchers {
  info("As a programmer")
  info("I want to be able to search the TVDB")
  info("So I can process the search results")

  feature("Searching") {
    val title = "Attack on Titan"
    scenario(s"A search query ('$title') is performed") {
      Given("An instance of a TvdbSearch")
      val searcher = new TvdbSearcher()
      When(s"I call search() with '$title'")
      val results = searcher.search(title)
      Then("I will receive a List of TvdbSearchResult classes")
      val result = TvdbSearchResult(
        showNames = List("en", "jp"),
        year = 2013,
        id = 19172
      )
      results should equal(List(result))
    }
  }
}