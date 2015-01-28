package org.excala

import java.io.{InputStream, OutputStream}

import com.github.nscala_time.time.Imports._
import org.excala.Errors._
import org.excala.StringUtils._

import scala.annotation.tailrec

/**
 * Created by Edmund on 2015-01-23.
 */
object Excala {

  trait TimeGetter {
    def getTime: DateTime
  }
  case class ImplicitDuration(duration: Duration)

  lazy val runtime = Runtime.getRuntime
  implicit val getProcessStdout = (_: Process).getInputStream
  implicit val getProcessStdin  = (_: Process).getOutputStream

  def exec(programName: String) = {
    val process = runtime.exec(programName)
    (process.getInputStream, process.getOutputStream)
  }

  def expect(str: String)(implicit stream: InputStream, timeout: ImplicitDuration) = {
    expectTimeout(str, timeout.duration)
  }

  def expectTimeout(str: String, timeout: Duration)(implicit stream: InputStream) = {
    expectDeadline(str, DateTime.now + timeout)
  }

  @tailrec
  def expectDeadline(str: String, deadline: DateTime)(implicit stream: InputStream): Result[Unit] = {
    if (str.isEmpty || isNull(str))
      win(())
    else if (DateTime.now > deadline)
      lose(ExpectTimedOut)
    else if (stream.read().toChar == str.head)
      expectDeadline(str.tail, deadline)
    else
      expectDeadline(str, deadline)
  }

  def sendLine(str: String)(implicit stream: OutputStream) = send(str + "\r\n")

  def send(str: String)(implicit stream: OutputStream) = win(
    stream write(str getBytes "ASCII")
  )
}
