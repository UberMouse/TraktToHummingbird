package nz.ubermouse.hummingbirdsyncer

import org.json4s.DefaultFormats

/**
 * Created by Taylor on 25/01/14.
 */
trait DefaultFormats {
  implicit val formats = DefaultFormats.lossless
}
