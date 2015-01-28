package org.excala.tests

import java.io.{PipedOutputStream, PipedInputStream, InputStream}

import com.github.nscala_time.time.Imports._
import org.excala.Errors._
import org.excala.Excala
import org.scalatest._

/**
 * Created by Edmund on 2015-01-24.
 */
class ExpectTests extends FlatSpec with Matchers {

  implicit def dummyInputStream: InputStream = new InputStream() {
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

  import org.excala.Excala._

  "Expects" should "time out after their timeouts" in {
    expectTimeout("Hello", 0.second)
  }

  "Expecting a nonempty String with zero timeout" should "fail immediately" in {
    val testString = "HELLO"
    expectTimeout(testString, 0.seconds) shouldBe lose(ExpectTimedOut)
  }

  "Expecting an empty String" should "return success" in {
    expectTimeout("", 0.seconds) shouldBe win(())
  }

  "Expecting null terminators" should "return success" in {
    expectTimeout("\0\0\0\0\0", 0.seconds) shouldBe win(())
  }

  "Expecting a string twice when it's received once" should "fail" in {
      val str = "HALLOO"
      implicit val stream = new StringOnceStream(str)
      assertResult(lose(ExpectTimedOut)) {
        for (start <- expectTimeout(str, 200.millis);
             end <- expectTimeout(str, 200.millis))
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
    sendLine(line)
    pipedIn read(buffer, 0, bufferLength)
    new String(buffer, "ASCII") shouldBe line + "\r\n"
    pipedIn read() shouldBe ' '
  }

}
