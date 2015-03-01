package org.excala.tests

import java.io._
import java.util.Date

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

  implicit val millis200 = ImplicitDuration(50.millis)

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

  case class StringOnceStream(val str: String) extends InputStream {
    var pos = 0

    override def read() = {
      val res = if (pos >= str.length) '\n'
      else str(pos)
      pos += 1
      res
    }

    override def available() = str.length - pos
  }

  case class StringForeverStream(val str: String) extends InputStream {
    var pos = 0

    override def read() = {
      val res = if (pos == str.length) '\n' else str(pos)
      pos += 1
      pos %= str.length + 1
      res
    }

    override def available() = str.length
  }

  case class DelayedStringStream(val str: String, val delay: Long) extends InputStream {
    val line = str + "\n"
    var pos = 0
    var startTime = none[Long]

    def start(): Unit = {
      startTime = Some(System.currentTimeMillis())
    }

    def isDone: Boolean = {
      startTime.exists(System.currentTimeMillis() - _ > delay)
    }

    override def read(): Int = {
      while (!isDone) {
        println("Delaying...")
      }
      val res = line(pos % line.length)
      pos += 1
      res
    }

    override def available(): Int = {
      if (isDone)
        1
      else
        0
    }

  }

  "Expects" should "not time out before they should" in {
    val str = "Hello"
    val stream = DelayedStringStream(str, 450)
    val timeout = 500 millis
    val regex = str.r
    stream.start()
    val result = stream.expectTimeout(regex, timeout)
    result should be a success
  }

  "Expects" should "time out after their timeouts" in {
    val str = "Hello"
    val stream = DelayedStringStream(str, 500)
    val timeout = 450 millis
    val regex = str.r
    stream.start()
    val result = stream.expectTimeout(regex, timeout)
    result should be a failure
  }


  "Expecting a nonempty Regex with zero timeout" should "fail immediately" in {
    nullInputStream.expectTimeout("HELLO".r, 0.seconds) should be a failure
  }

  "Expecting an empty String" should "return success" in {
    nullInputStream.expect("") should be a success
  }

  "Expecting null terminators" should "return success" in {
    nullInputStream.expect("\0\0\0\0\0") should be a success
  }

  "Chaining successes" should "return success" in {
    chain(win(""), win("")) should be a success
  }

  "Chaining a success and a failure" should "return failure" in {
    chain(win(""), lose(EOF)) should be a failure
  }

  "Chaining two failures" should "return failure" in {
    chain(lose(EOF), lose(EOF)) should be a failure
  }

  "Expecting a string twice when it's received once" should "fail" in {
    val str = "HALLOO"
    val stream = StringOnceStream(str)
    chain(stream expect str, stream expect str) should be a failure
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
    val stream = StringForeverStream(str)
    val result = stream expect str
    result should be a success
  }

  "Expecting a regex" should "work when it's sent" in {
    val regex = "[0-9]+".r
    val stream = StringForeverStream("1234")
    val result = stream expectTimeout (regex, 1 second)
    result should be a success
    result map(_ shouldBe "1234")
  }

}
