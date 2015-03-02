package org.excala

import java.io._

import com.github.nscala_time.time.Implicits._
import org.joda.time.{DateTime, Duration}

import scala.annotation.tailrec
import scala.util.matching._
import scalaz._

/**
 * Created by Edmund on 2015-02-23.
 */
trait ExpectableImplicits {
  self: Errors =>

  implicit object ProcessExpectable extends Expectable[Process] {
    def outStream(f: Process) = f.getOutputStream

    def inStream(f: Process) = f.getInputStream

    def alive(f: Process) = f.isAlive
  }

  val BusyWaitPeriod: Long = 15

  implicit class ExpectableOps[-F: Expectable](f: F) {
    def inStream: InputStream = implicitly[Expectable[F]].inStream(f)

    def outStream: OutputStream = implicitly[Expectable[F]].outStream(f)

    def alive: Boolean = implicitly[Expectable[F]].alive(f)

    def read(): Char = inStream.read().toChar

    def write(i: Int) = outStream.write(i)

    def available: Int = inStream.available

    final def waitForLine(deadline: Long): Result[String] = {
      @tailrec def go(soFar: Cord): Result[Cord] = {
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

    private def expectTimeout(regex: Regex, timeout: Duration): Result[String] = {
      val deadline = System.currentTimeMillis() + timeout.getMillis

      @tailrec def go(regex: Regex): Result[String] = {
        // I apologize in advance for the ugliness of this method.
        // I realized that if a line were long enough, this function would result
        // in a stack overflow if it were not tail recursive. Scala can't optimize
        // tail recursion with higher order functions, so I did what I could.
        // TODO: Fix with trampolines.
        val line = waitForLine(deadline)
        if (line.isLeft)
          line
        else {
          val regexMatch = line.fold(_ => ???, regex.findFirstIn)
          if (!regexMatch.isDefined)
            go(regex)
          else
            win(regexMatch.get)
        }
      }

      go(regex)
    }

    def expect(str: String)(implicit timeout: Duration): Result[String] = {
      if (str.isEmpty ||
        str.count(ch => Character.isWhitespace(ch) || ch == '\0') == str.length)
        win(str)
      else
        expectTimeout(new Regex(Regex.quote(str)), timeout)
    }

    def expect(rgx: Regex)(implicit timeout: Duration): Result[String] =
      expectTimeout(rgx, timeout)

    def expectLine(implicit timeout: Duration): Result[String] = {
      waitForLine(System.currentTimeMillis() + timeout.getMillis)
    }

    def sendLine(str: String) = send(str + "\r\n")

    def send(str: String) = outStream write (str getBytes "UTF-8")

  }

}
