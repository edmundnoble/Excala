package org.excala

import java.util.Date

import org.joda.time.Duration

import scalaz._
import Scalaz._

/**
 * Your one-stop shop for imports.
 * Created by Edmund on 2015-01-23.
 */

object Excala extends Errors with ExpectableImplicits {

  case class ImplicitDuration(duration: Duration)

  trait TimeGetter {
    def getTime: Date
  }

  lazy val runtime = Runtime.getRuntime

  def exec(programName: String): Process = runtime.exec(programName)

  case class CBN[+A](f: () => A)

  implicit def cbn2CBN[A](a: => A): CBN[A] = CBN(() => a)

  def chain[A](funs: CBN[Result[A]]*): Result[A] = {
    val otherfuns = funs.map(_.f)
    otherfuns.tail.foldLeft(otherfuns.head())(_ >> _())
  }
}