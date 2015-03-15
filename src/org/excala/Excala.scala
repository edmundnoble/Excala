package org.excala

import java.util.Date

import org.joda.time.Duration

import scalaz._
import Scalaz._

/**
 * Your one-stop shop for imports.
 */

object Excala extends Errors with ExpectableImplicits {

  trait TimeGetter {
    def getTime: Date
  }

  lazy val runtime = Runtime.getRuntime

  def exec(programName: String): Process = runtime.exec(programName)

  case class CBN[+A](f: () => A) extends AnyVal

  implicit def cbn2CBN[A](a: => A): CBN[A] = CBN(() => a)

  def chain[A](funs: CBN[Result[A]]*): Result[A] = {
    val otherfuns = funs.map(_.f)
    otherfuns.tail.foldLeft(otherfuns.head())(_ >> _())
  }
}