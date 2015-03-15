package org.excala

import scala.annotation.tailrec

/**
 * Created by Edmund on 2015-01-25.
 */
object StringUtils {
  @inline
  def isNull(str: String) =
    str.isEmpty || str.forall(_ == '\0')
}
