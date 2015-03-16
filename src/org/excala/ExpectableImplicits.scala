package org.excala

import java.io._

import com.github.nscala_time.time.Implicits._
import org.joda.time.{DateTime, Duration}

import scala.util.matching._
import scalaz.Free.Trampoline
import scalaz._
import Scalaz._
import Trampoline._

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

    private def waitForLine(deadline: Long): Result[String] = {
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

      def go(): Trampoline[Result[List[String]]] = {
        val line = waitForLine(deadline)
        val regexMatch = line.map(regex.findFirstMatchIn(_))
        regexMatch match {
          case \/-(Some(rMatch)) => done(win(rMatch.subgroups))
          case \/-(None) => suspend(go())
          case -\/(err) => done(lose(err))
        }
      }
      go().run
    }

    private def expectTimeout[A](string: String)(timeout: Duration): Result[String] = {
      val deadline = System.currentTimeMillis() + timeout.getMillis

      def go(): Trampoline[Result[String]] = {
        val line = waitForLine(deadline)
        line match {
          case \/-(ln) =>
            if (ln.contains(string))
              done(win(ln))
            else
              suspend(go())
          case -\/(err) =>
            done(lose(err))
        }
      }
      go().run
    }

    private def expectTimeout[A](strings: (String, String))(timeout: Duration): (Result[String], Result[String]) = {
      val deadline = System.currentTimeMillis() + timeout.getMillis

      def go(): Trampoline[(Result[String], Result[String])] = {
        val line = waitForLine(deadline)
        line match {
          case \/-(ln) =>
            if (ln.contains(strings._1))
              done(win(ln), lose(OtherExpect))
            else if (ln.contains(strings._2))
              done(lose(OtherExpect), win(ln))
            else
              suspend(go())
          case -\/(err) =>
            done(lose(err), lose(err))
        }
      }
      go().run
    }

    def expect(str: String)(implicit timeout: Duration): Result[String] = {
      if (StringUtils.isNull(str))
        win(str)
      else
        expectTimeout(str)(timeout)
    }

    def expect(strs: (String, String))(implicit timeout: Duration): (Result[String], Result[String]) = {
      if (StringUtils.isNull(strs._1))
        (win(strs._1), lose(OtherExpect))
      else if (StringUtils.isNull(strs._2))
        (lose(OtherExpect), win(strs._2))
      else
        expectTimeout(strs)(timeout)
    }

    def expect(rgx: Regex)(implicit timeout: Duration): Result[List[String]] =
      expectTimeout(rgx, timeout)

    def expectLine(implicit timeout: Duration): Result[String] =
      waitForLine(System.currentTimeMillis() + timeout.getMillis)


    def sendLine(str: String): Result[Unit] = send(str + "\r\n")

    def send(str: String): Result[Unit] = {
      outStream write (str getBytes "UTF-8")
      win(())
    }

  }

}
