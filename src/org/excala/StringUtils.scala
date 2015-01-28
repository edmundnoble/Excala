package org.excala

import scala.annotation.tailrec

/**
 * Created by Edmund on 2015-01-25.
 */
object StringUtils {
  def isNull(str: String) = {
    @tailrec
    def isCharListNull(list: List[Char], soFar: Boolean = true): Boolean = {
      list match {
        case Nil => soFar
        case '\0' :: xs => isCharListNull(xs, soFar)
        case other => false
      }
    }
    isCharListNull(str.toList)
  }
}
