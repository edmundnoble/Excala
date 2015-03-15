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

  val ExpectCount = 100000L

  val str = "Testing 123"

  val stream = new StringForeverStream(str)

  var i = 0

  val start = System.currentTimeMillis()
  var result = win("")
  while (i < ExpectCount) {
    implicit val timeout: Duration = (100 millis).toDuration
    result = chain(result, stream expect str)
    i += 1
  }
  val finish = System.currentTimeMillis()
  val elapsedPerExpect = (finish - start) / ExpectCount.toDouble
  println(s"Elapsed time for $ExpectCount expects: ${finish - start} millis")
  println(s"Elapsed time per expect: $elapsedPerExpect millis")
}
