package org.excala

import java.io._

import com.github.nscala_time.time.Implicits._
import org.excala.Excala.ImplicitDuration
import org.joda.time.{DateTime, Duration}

import scala.annotation.tailrec
import scala.util.matching._
import scalaz._

/**
 * Created by Edmund on 2015-02-23.
 */
trait ExpectableImplicits {
  self : Errors =>

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

  implicit object ProcessExpectable extends Expectable[Process] {
    def outStream(f: Process) = f.getOutputStream

    def inStream(f: Process) = f.getInputStream

    def alive(f: Process) = f.isAlive
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
      def go(regex: Regex): Result[String] =
        for (line <- waitForLine(deadline.toDate.getTime);
             res <- regex.findFirstIn(line) match {
               case Some(s) => win(s)
               case None => go(regex)
             }) yield res

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
