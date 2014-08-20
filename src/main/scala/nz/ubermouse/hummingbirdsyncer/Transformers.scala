package nz.ubermouse.hummingbirdsyncer

import _root_.api.Hummingbird.{ValidMapping, HummingbirdShow}
import nz.ubermouse.hummingbirdsyncer.api.Trakt.TraktActivity

/**
 * Created by Taylor on 10/12/13.
 */
object Transformers {
  val highestEpisode = (x:List[TraktActivity]) => {
    val highestPerSeason = {
      x.groupBy(_.episode.season)
       .map{case(_, activities:List[TraktActivity]) => activities.sortWith((x, y) => x.episode.episode > y.episode.episode).head}
       .toList
    }
    highestPerSeason.sortWith((x, y) => x.episode.season > y.episode.season).head
  }

  val showRequiresSync = (x:TraktActivity, library:List[HummingbirdShow]) => {
    library.exists(y => y.anime.slug.toLowerCase == x.show.slug.toLowerCase
                     && y.episodes_watched < x.episode.episode)
  }
  val overrideShowNames = (x:TraktActivity, getMapping: Int => Option[ValidMapping]) => {
    x.copy(x.show.copy(slug = getMapping(x.show.tvdb_id).map(x => x.OverrideSlug).getOrElse(x.show.slug)))
  }

  val fixGeneric = (x:TraktActivity, mapExtractor: ValidMapping => Map[String, String], keyExtractor: TraktActivity => String, getMapping: Int => Option[ValidMapping]) => {
    getMapping(x.show.tvdb_id).flatMap(y => mapExtractor(y).get(keyExtractor(x)))
                              .map(slug => x.copy(x.show.copy(slug = slug)))
                              .getOrElse(x)
  }

  val fixSeasons = (x:TraktActivity, getMapping: Int => Option[ValidMapping]) => fixGeneric(x,
                                                                                           (m:ValidMapping) => m.SeasonOverrides,
                                                                                           (t:TraktActivity) => t.episode.season.toString(),
                                                                                           getMapping)

  val fixSpecials = (x:TraktActivity, getMapping: Int => Option[ValidMapping]) => {
    x.episode.season.toInt match {
      case 0 => fixGeneric(x,
                          (m:ValidMapping) => m.SpecialOverrides,
                          (t:TraktActivity) => t.episode.episode.toString(),
                          getMapping)
      case _ => x
    }
  }

  val fixEpisodes = (x: TraktActivity, getMapping: Int => Option[ValidMapping]) => {
    getMapping(x.show.tvdb_id).map(mapping => mapping.SeasonOverrides)
                              .filter(mapping => mapping.contains(x.episode.season.toString()))
                              .filter(mapping => mapping(x.episode.season.toString()).matches("\\+[0-9]+"))
                              .map(mapping => mapping(x.episode.season.toString()).drop(1).toInt)
                              .map(epInc => x.copy(episode = x.episode.copy(episode = x.episode.episode + epInc, season = 1)))
                              .getOrElse(x)
  }
}
