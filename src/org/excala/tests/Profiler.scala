package org.excala.tests

import java.io.InputStream

import com.github.nscala_time.time.Imports._
import org.excala.Excala._

/**
 * Measures the performance of individual expects, assuming there is no delay between lines.
 */
object Profiler extends App with TestImplicits {

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

  val str = "Testing 123"

  val stream = new StringForeverStream(str)

  var i = 0

  val start = System.currentTimeMillis()
  var result = win("")
  while (i < 100000) {
    implicit val timeout: Duration = (100 millis).toDuration
    chain(result, stream.expect(str))
    i += 1
  }
  println((System.currentTimeMillis() - start) / 100000.0)
}
