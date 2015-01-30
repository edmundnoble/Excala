package org.excala

import java.io.{InputStream, OutputStream}

import com.github.nscala_time.time.Imports._
import scala.language.implicitConversions

/**
 * Created by Edmund on 2015-01-23.
 */

object Excala extends ExpectableImplicits {

  case class ImplicitDuration(duration: Duration)
  trait TimeGetter {
    def getTime: DateTime
  }

  lazy val runtime = Runtime.getRuntime

  def exec(programName: String) = runtime.exec(Array(programName))
}