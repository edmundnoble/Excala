package org.excala.tests

import java.io._

import com.github.nscala_time.time.Imports._
import org.excala.Excala._
import org.scalatest._
import org.scalatest.matchers.{BePropertyMatchResult, BePropertyMatcher}

import scalaz._
import Scalaz._

/**
 * Tests for expect(...) and variants.
 */
class ExpectTests extends ExpectTestSpec with TestImplicits {
  "An Expect" should "not time out before it should" taggedAs TimedTest in {
    val str = "Hello"
    val stream = DelayedStringStream(str, 450)
    implicit val timeout: Duration = 500 millis
    val regex = str.r
    stream.start()
    val result = stream expect regex
    result should be a success
  }

  it should "time out after its timeout" taggedAs TimedTest in {
    val str = "Hello"
    val stream = DelayedStringStream(str, 500)
    implicit val timeout: Duration = 450 millis
    val regex = str.r
    stream.start()
    val result = stream expect regex
    result should be a failure
  }

  "Expecting a nonempty Regex with zero timeout" should "fail immediately" in {
    import ZeroDuration._
    nullInputStream expect "HELLO".r should be a failure
  }

  "Expecting an empty String" should "return success" in {
    import ZeroDuration._
    nullInputStream expect "" should be a success
  }

  "Expecting null terminators" should "return success" in {
    import ZeroDuration._
    nullInputStream expect "\0\0\0\0\0" should be a success
  }

  "Expecting a string twice when it's received once" should "fail" in {
    import ShortDuration._
    val str = "HALLOO"
    val stream = StringOnceStream(str)
    stream expect str should be a success
    stream expect str should be a failure
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
    implicit val timeout: Duration = 5 seconds
    val stream = StringForeverStream(str)
    val result = stream expect str
    result should be a success
  }

  "Expecting a regex" should "work when it's sent" in {
    val regex = """([0-9]+)""" r
    val stream = StringForeverStream("1234")
    implicit val timeout: Duration = 5 seconds
    val result = stream expect regex
    result should be a success
    result map (_ shouldBe List("1234"))
  }

  "One complicated-ass expect" should "work" in {
    import ShortDuration._
    val stream = StringListStream("Hello!", "My name is:", "Edmund")
    val n = for (name <- chain(stream expect "Hello",
      stream expect "name is",
      stream expectLine)) yield name
    n should be a success
    n map {
      _ shouldBe "Edmund\n"
    }
  }
}
