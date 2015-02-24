package org.excala

import scala.language.implicitConversions
import scalaz.Scalaz._
import scalaz._

/**
 * Created by Edmund on 2015-01-23.
 */
object Errors {
  sealed abstract class Error

  case object ExpectTimedOut extends Error
  case object EOF extends Error
  case class ExceptionContainer(ex: RuntimeException) extends Error

  implicit val ErrorEqual = new Equal[Error] {
    def equal(a: Error, b: Error) = a match {
      case ExpectTimedOut => b == ExpectTimedOut
      case EOF => b == EOF
      case ExceptionContainer(ex) => b match {
        case ExceptionContainer(ex2) => ex == ex2
        case _ => false
      }
    }
  }

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
