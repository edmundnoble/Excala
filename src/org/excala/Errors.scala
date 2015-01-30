package org.excala

import scala.language.implicitConversions
import scalaz.Scalaz._
import scalaz._

/**
 * Created by Edmund on 2015-01-23.
 */
object Errors {
  sealed trait Error

  case object ExpectTimedOut extends Error
  case class ExceptionContainer(ex: RuntimeException) extends Error

  implicit def ErrorToString(err: Error): String = {
    err match {
      case ExceptionContainer(ex: Exception) =>
        ex.printStackTrace(); ""

      case ExpectTimedOut => "Expect timed out!"
    }
  }

  type Result[+S] = \/[Error, S]

  def lose[S](err: Error) = err.left[S]

  def win[S](result: S) = result.right[Error]

}
