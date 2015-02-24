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
 * Created by Edmund on 2015-02-23.
 */
class ExpectableImplicits {

  implicit object OutputStreamExpectable extends Expectable[OutputStream] {
    def outStream(f: OutputStream) = f

    def inStream(f: OutputStream) = null

    def alive(f: OutputStream) = true
  }

  implicit object InputStreamExpectable extends Expectable[InputStream] {
    def outStream(f: InputStream) = null

    def inStream(f: InputStream) = f

    def alive(f: InputStream) = true
  }

  val BusyWaitPeriod: Long = 15

  implicit class SlaveOps[-F: Expectable](f: F) {
    def inStream: InputStream = implicitly[Expectable[F]].inStream(f)

    def outStream: OutputStream = implicitly[Expectable[F]].outStream(f)

    def alive: Boolean = implicitly[Expectable[F]].alive(f)

    def read(): Char = inStream.read().toChar

    def write(i: Int) = outStream.write(i)

    def available: Int = inStream.available


    final def waitForLine(deadline: Long): Result[String] = {
      @tailrec
      def go(soFar: Cord): Result[Cord] = {
        if (System.currentTimeMillis() > deadline) {
          lose(ExpectTimedOut)
        }
        else {
          if (available == 0) {
            Thread.sleep(BusyWaitPeriod)
            go(soFar)
          } else {
            val readChar = read()
            if (readChar == '\n')
              win(soFar :- readChar)
            else
              go(soFar :- readChar)
          }
        }
      }
      go(Cord.empty).map(_.toString())
    }

    def expectDeadline(regex: Regex, deadline: DateTime): Result[String] = {
      def go(regex: Regex): Result[String] = {
        val line = waitForLine(deadline.toDate.getTime)
        line.flatMap(regex.findFirstIn(_) match {
          case Some(s) => win(s)
          case None => go(regex)
        })
      }
      go(regex)
    }

    def expectTimeout(regex: Regex, timeout: Duration) = expectDeadline(regex, DateTime.now + timeout)

    def expect(str: String)(implicit timeout: ImplicitDuration): Result[String] = {
      if (str == "" ||
        str.toList.count(ch => Character.isWhitespace(ch) || ch == '\0') == str.length)
        win(str)
      else
        expectTimeout(new Regex(Regex.quote(str)), timeout.duration)
    }

    def sendLine(str: String) = send(str + "\r\n")

    def send(str: String) = outStream write (str getBytes "UTF-8")

  }

}
