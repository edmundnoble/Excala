package org.excala.tests

import java.io._

import com.github.nscala_time.time.Imports._
import org.excala.Excala._
import org.scalatest._
import org.scalatest.matchers.{BePropertyMatchResult, BePropertyMatcher}

import scalaz.Scalaz._
import scalaz._

/**
 * Created by Edmund on 2015-01-24.
 */
trait ExpectTestSpec extends FlatSpec with Matchers with ExpectTags {

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

  case class StringOnceStream(str: String) extends InputStream {
    var pos = 0

    override def read() = {
      val res = if (pos >= str.length) '\n'
      else str(pos)
      pos += 1
      res
    }

    override def available() = str.length - pos
  }

  case class StringForeverStream(str: String) extends InputStream {
    var pos = 0

    override def read() = {
      val res = if (pos == str.length) '\n' else str(pos)
      pos += 1
      pos %= str.length + 1
      res
    }

    override def available() = str.length
  }

  case class DelayedStringStream(str: String, delay: Long) extends InputStream {
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

}
