package org.excala

import java.io.{InputStream, OutputStream}

import com.github.nscala_time.time.Imports._
import org.excala.Errors._
import org.excala.Excala.ImplicitDuration
import org.excala.StringUtils._

import scala.annotation.tailrec

/**
 * Created by Edmund on 2015-01-28.
 */
trait Expectable[-F] {
  def inStream(f: F): InputStream
  def outStream(f: F): OutputStream
  def alive(f: F): Boolean


}

class ExpectableImplicits {

  implicit object ProcessExpectable extends Expectable[Process] {
    def inStream(f: Process) = f.getInputStream
    def outStream(f: Process) = f.getOutputStream
    def alive(f: Process) = f.isAlive
  }

  implicit class SlaveOps[-F : Expectable](f: F) {
    def inStream = implicitly[Expectable[F]].inStream(f)
    def outStream = implicitly[Expectable[F]].outStream(f)
    def alive = implicitly[Expectable[F]].alive(f)
    def expect(str: String)(implicit timeout: ImplicitDuration) = {
      expectTimeout(str, timeout.duration)
    }

    def expectTimeout(str: String, timeout: Duration) = {
      expectDeadline(str, DateTime.now + timeout)
    }

    @tailrec
    final def expectDeadline(str: String, deadline: DateTime): Result[Unit] = {
      if (str.isEmpty || isNull(str))
        win(())
      else if (DateTime.now > deadline)
        lose(ExpectTimedOut)
      else if (implicitly[Expectable[F]].inStream(f).read().toChar == str.head)
        expectDeadline(str.tail, deadline)
      else
        expectDeadline(str, deadline)
    }

    def sendLine(str: String) = send(str + "\r\n")

    def send(str: String) = win(
      implicitly[Expectable[F]].outStream(f).write(str getBytes "ASCII")
    )
  }


}