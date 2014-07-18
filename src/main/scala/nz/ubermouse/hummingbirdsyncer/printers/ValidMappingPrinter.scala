package nz.ubermouse.hummingbirdsyncer.printers

import api.Hummingbird.ValidMapping
import com.typesafe.scalalogging.slf4j.Logging

/**
 * Created by Taylor on 18/07/14.
 */
object ValidMappingPrinter extends Logging {

  def stringRepresentation(mapping: ValidMapping) = {
    var seasonOverrides = ""
    for(so <- mapping.SeasonOverrides)
      seasonOverrides += s"${so._1}: ${so._2}\\n"

    var specialOverrides = ""
    for(so <- mapping.SpecialOverrides)
      specialOverrides += s"${so._1}: ${so._2}\\n"
    
    s"""
      | ${mapping.OverrideSlug} (${mapping.TvDBId})
        - Season Overrides:
          $seasonOverrides
        - Special Overrides:
          $specialOverrides
    """.stripMargin   
  }

  def apply(mappings:List[ValidMapping]) {
    for(mapping <- mappings)
      logger.debug(stringRepresentation(mapping))
  }
  
  def apply(mapping:ValidMapping):Unit = apply(List(mapping))
}
