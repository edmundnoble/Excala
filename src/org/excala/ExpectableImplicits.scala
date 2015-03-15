package org.excala

import java.io._

import com.github.nscala_time.time.Implicits._
import org.joda.time.{DateTime, Duration}

import scala.annotation.tailrec
import scala.util.control.TailCalls
import scala.util.control.TailCalls._
import scala.util.matching._
import scalaz._
import Scalaz._

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
      val builder = new StringBuilder()
      while (deadline - System.currentTimeMillis > 0) {
        if (available == 0) {
          Thread.sleep(Math.max(BusyWaitPeriod, (deadline - System.currentTimeMillis) / 10L))
        } else {
          val readChar = read()
          builder.append(readChar)
          if (readChar == '\n')
            return win(builder.toString())
        }
      }
      lose(ExpectTimedOut)
    }

    private def expectTimeout(regex: Regex, timeout: Duration): Result[List[String]] = {
      val deadline = System.currentTimeMillis() + timeout.getMillis

      def go(): TailRec[Result[List[String]]] = {
        val line = waitForLine(deadline)
        val regexMatch = line.map(regex.findFirstMatchIn(_))
        regexMatch match {
          case \/-(Some(rMatch)) => done(win(rMatch.subgroups))
          case \/-(None) => tailcall(go())
          case -\/(err) => done(lose(err))
        }
      }
      go().result
    }

    private def expectTimeout[A](string: String)(timeout: Duration): Result[String] = {
      val deadline = System.currentTimeMillis() + timeout.getMillis

      def go(): TailRec[Result[String]] = {
        val line = waitForLine(deadline)
        line match {
          case \/-(ln) =>
            if (ln.contains(string))
              done(win(ln))
            else
              tailcall(go())
          case -\/(err) =>
            done(lose(err))
        }
      }
      go().result
    }

    def expect(str: String)(implicit timeout: Duration): Result[String] = {
      if (str.isEmpty ||
        str.count(ch => Character.isWhitespace(ch) || ch == '\0') == str.length)
        win(str)
      else
        expectTimeout(str)(timeout)
    }

    def expect(rgx: Regex)(implicit timeout: Duration): Result[List[String]] =
      expectTimeout(rgx, timeout)

    def expectLine(implicit timeout: Duration): Result[String] = {
      waitForLine(System.currentTimeMillis() + timeout.getMillis)
    }

    def sendLine(str: String): Result[Unit] = send(str + "\r\n")

    def send(str: String): Result[Unit] = {
      outStream write (str getBytes "UTF-8")
      win(())
    }

  }

}
