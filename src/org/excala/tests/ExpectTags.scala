package org.excala.tests

import org.scalatest.Tag

/**
 * Contains all of the tags for tests that we'll use.
 */
trait ExpectTags {
  object TimedTest extends Tag("org.excala.TimedTest")
}
