package org.excala.tests

import java.io._

import com.github.nscala_time.time.Imports._
import org.excala.Errors._
import org.excala.Excala._
import org.excala._
import org.scalatest._
import org.scalatest.matchers.{BePropertyMatchResult, BePropertyMatcher}
import org.scalatest.concurrent.Eventually._
import scala.util.matching._

import scalaz._
import Scalaz._

/**
 * Created by Edmund on 2015-01-24.
 */
class ExpectTests extends FlatSpec with Matchers {

  implicit val millis200 = ImplicitDuration(200.millis)

  def failure[A, B] = new BePropertyMatcher[A \/ B] {
    def apply(dis: A \/ B) = BePropertyMatchResult(dis.isLeft, "left value")
  }
  def success[A, B] = new BePropertyMatcher[A \/ B] {
    def apply(dis: A \/ B) = BePropertyMatchResult(dis.isRight, "right value")
  } 

  def nullInputStream: InputStream = new InputStream() {
    override def read() = '0'
    override def available() = 1
  }

  class StringOnceStream(val str: String) extends InputStream {
    var pos = 0
    override def read() = {
      val res = if (pos >= str.length) '\n'
                else str(pos)
      pos += 1
      res
    }
    override def available() = str.length - pos
  }

  class StringForeverStream(val str: String) extends InputStream {
    var pos = 0
    override def read() = {
      val res = if (pos == str.length) '\n' else str(pos)
      pos += 1
      pos %= str.length + 1
      res
    }
    override def available() = str.length
  }

  "Expects" should "time out after their timeouts" ignore {
    val stream = nullInputStream
    val mils = 100
    val timeout = mils millis
    val tolerance = 20
    val regex = "Hello".r
    val then = System.currentTimeMillis()
    stream.expectTimeout(regex, timeout) shouldBe a(failure)
    val now = System.currentTimeMillis()
    val diff = now - then
    diff shouldBe mils +- tolerance
  }

  "Expecting a nonempty Regex with zero timeout" should "fail immediately" in {
    nullInputStream.expectTimeout("HELLO".r, 0.seconds) shouldBe lose(ExpectTimedOut)
  }

  "Expecting an empty String" should "return success" in {
    nullInputStream.expect("") shouldBe a (success)
  }

  "Expecting null terminators" should "return success" in {
    nullInputStream.expect("\0\0\0\0\0") shouldBe a (success)
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

  "Expecting a string" should "work when it's sent" in {
    val str = "Test"
    val stream = new StringForeverStream(str)
    val result = stream.expect(str)
    result should be a success
  }

  "Expecting a regex" should "work when it's sent" in {
    val regex = "[0-9]+".r
    val stream = new StringForeverStream("1234")
    val result = stream.expectTimeout(regex, 1 second)
    result should be a success
    result.map(_ shouldBe "1234")
  }

}
