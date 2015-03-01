package org.excala.tests

import org.scalatest.Tag

/**
 * Contains all of the tags for tests that we'll use.
 * Created by Edmund on 2015-03-01.
 */
trait ExpectTags {
  object TimedTest extends Tag("org.excala.TimedTest")
}
