package org.excala.tests

import java.io._

import com.github.nscala_time.time.Imports._
import org.excala.Excala._

import scalaz._
import Scalaz._

/**
 * Tests for operators like chain(...).
 * Created by Edmund on 2015-03-01.
 */
class OpsTests extends ExpectTestSpec {
  "Chaining successes" should "return success" in {
    chain(win(""), win("")) should be a success
  }

  "Chaining a success and a failure" should "return failure" in {
    chain(win(""), lose(EOF)) should be a failure
  }

  "Chaining two failures" should "return failure" in {
    chain(lose(EOF), lose(EOF)) should be a failure
  }

  "Chaining a failure with a function" should "not run the function" in {
    var x = 0
    def increment = {x += 1; win(())}
    chain(lose(EOF), increment)
    x should equal (0)
  }

 "Chaining two successful functions" should "run each once" in {
   var x = 0
   def increment = {x += 1; win(())}
   chain(increment, increment)
   x should equal(2)
 }
}
