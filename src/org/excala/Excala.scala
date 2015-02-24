package org.excala

import java.io.{InputStream, OutputStream}
import java.nio.file._
import java.util.Date

import java.nio.file.StandardWatchEventKinds._

import org.joda.time.Duration

/**
 * Created by Edmund on 2015-01-23.
 */

object Excala extends ExpectableImplicits {

  case class ImplicitDuration(duration: Duration)
  trait TimeGetter {
    def getTime: Date
  }

  lazy val runtime = Runtime.getRuntime

  def exec(programName: String): Process = runtime.exec(programName)
}