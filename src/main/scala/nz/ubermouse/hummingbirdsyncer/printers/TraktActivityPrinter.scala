package nz.ubermouse.hummingbirdsyncer.printers

import nz.ubermouse.hummingbirdsyncer.api.Trakt.TraktActivity
import com.typesafe.scalalogging.slf4j.Logging

/**
 * Created by Taylor on 18/07/14.
 */
object TraktActivityPrinter extends Logging {

  def apply(activities:List[TraktActivity]) {
    for(activity <- activities)
      logger.trace(stringRepresentation(activity))
  }

  def apply(activity:TraktActivity):Unit = apply(List(activity))

  def stringRepresentation(activity: TraktActivity) = {
    s"""
      | ${activity.show.title} (${activity.show.slug})
        - Tvdb Id: ${activity.show.tvdb_id}
        - Season: ${activity.episode.season}
        - Episode: ${activity.episode.episode}
    """.stripMargin
  }
}
