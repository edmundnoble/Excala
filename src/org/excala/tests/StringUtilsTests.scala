package org.excala.tests

import org.excala.StringUtils

/**
 * Created by Edmund on 2015-03-15.
 */
class StringUtilsTests extends ExpectTestSpec {
  "isNull(String)" should "check if the string is null" in {
    val nulls = "\0\0\0\0\0"
    StringUtils.isNull(nulls) shouldBe true
    val noNulls = "Hello"
    StringUtils.isNull(noNulls) shouldBe false
    val someNulls = "Hello\0"
    StringUtils.isNull(someNulls) shouldBe false

  }
}
