package org.excala.tests

import java.io._

import com.github.nscala_time.time.Imports._
import org.excala.Errors._
import org.excala.Excala._
import org.excala._
import org.scalatest._

/**
 * Created by Edmund on 2015-01-24.
 */
class ExpectTests extends FlatSpec with Matchers {

  implicit val millis200 = ImplicitDuration(200.millis)

  def nullInputStream: InputStream = new InputStream() {
    override def read() = '0'
  }

  class StringOnceStream(val str: String) extends InputStream {
    var pos = 0
    override def read() = {
      val res = if (pos >= str.length) '\0'
                else str(pos)
      pos += 1
      res
    }
  }

  class StringForeverStream(val str: String) extends InputStream {
    var pos = 0
    override def read() = {
      val res = str(pos % str.length)
      pos += 1
      res
    }
  }

  implicit object OutputStreamExpectable$ extends Expectable[OutputStream] {
    def outStream(f: OutputStream) = f
    def inStream(f: OutputStream) = null
    def alive(f: OutputStream) = true
  }

  implicit object InputStreamExpectable$ extends Expectable[InputStream] {
    def outStream(f: InputStream) = null
    def inStream(f: InputStream) = f
    def alive(f: InputStream) = true
  }

  import org.excala.Excala._

  "Expects" should "time out after their timeouts" in {
    val stream = nullInputStream
    stream.expectTimeout("Hello", 0.second)
  }

  "Expecting a nonempty String with zero timeout" should "fail immediately" in {
    nullInputStream.expectTimeout("HELLO", 0.seconds) shouldBe lose(ExpectTimedOut)
  }

  "Expecting an empty String" should "return success" in {
    nullInputStream.expectTimeout("", 0.seconds) shouldBe win(())
  }

  "Expecting null terminators" should "return success" in {
    nullInputStream.expectTimeout("\0\0\0\0\0", 0.seconds) shouldBe win(())
  }

  "Expecting a string twice when it's received once" should "fail" in {
      val str = "HALLOO"
      val stream = new StringOnceStream(str)
      assertResult(lose(ExpectTimedOut)) {
        for (start <- stream.expect(str);
             end <- stream.expect(str))
        yield end
      }
  }

  "Sending a line" should "send the line and a newline" in {
    val pipedIn = new PipedInputStream()
    implicit val pipedOut = new PipedOutputStream(pipedIn)
    val line = "HELLO!"
    val lineLength = line.length
    val bufferLength = lineLength + 2
    val buffer = new Array[Byte](bufferLength)
    pipedOut sendLine line
    pipedIn read(buffer, 0, bufferLength)
    new String(buffer, "ASCII") shouldBe line + "\r\n"
  }

  "Reading an EOF" should "return an EOF error" in {
    val EOF = -1.toChar
  }

}
