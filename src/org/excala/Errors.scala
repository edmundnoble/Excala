package org.excala

import scala.language.implicitConversions
import scalaz.Scalaz._
import scalaz._

trait Errors {

  sealed abstract class Error

  case object ExpectTimedOut extends Error

  case object EOF extends Error

  case class ExceptionContainer(ex: RuntimeException) extends Error

  implicit def ErrorToString(err: Error): String = {
    err match {
      case ExceptionContainer(ex) => (ex getStackTrace) mkString "\n"

      case ExpectTimedOut => "Expect timed out!"

      case EOF => "End of File!"
    }
  }

  type Result[+S] = \/[Error, S]

  def lose[S](err: Error) = err.left[S]

  def win[S](result: S) = result.right[Error]

}
