package org.excala

import scala.annotation.tailrec

object StringUtils {
  @inline
  def isNull(str: String) =
    str.isEmpty || str.forall(_ == '\0')
}
