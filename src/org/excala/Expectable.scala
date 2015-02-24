package org.excala

import java.io._
import java.nio._
import java.nio.channels.{AsynchronousFileChannel, AsynchronousChannel}
import java.nio.channels.spi.AbstractInterruptibleChannel
import java.nio.file.WatchService
import java.util.regex.Pattern
import java.util.{Scanner, Date}
import com.github.nscala_time.time.Implicits._

import org.excala.Errors._
import org.excala.Excala.ImplicitDuration
import org.excala.StringUtils._
import org.joda.time.{Duration, DateTime}

import scala.util.matching._

import scala.annotation.tailrec

import scalaz._
import Scalaz._

/**
 * Created by Edmund on 2015-01-28.
 */
trait Expectable[-F] {
  def inStream(f: F): InputStream

  def outStream(f: F): OutputStream

  def alive(f: F): Boolean
}