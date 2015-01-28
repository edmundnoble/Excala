package org.excala

import com.github.nscala_time.time.Imports._
import org.excala.Errors._
import org.excala.StringUtils._

/**
 * Created by Edmund on 2015-01-26.
 */
object Implicits {
  implicit val defaultTimeout = 200.millis
}
