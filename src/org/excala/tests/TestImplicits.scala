package org.excala.tests

import java.io.{InputStream, OutputStream}

import org.excala.Expectable

/**
 * Completely unsafe, only for testing purposes.
 */
private[tests] trait TestImplicits {
  implicit object OutputStreamExpectable extends Expectable[OutputStream] {
    def outStream(f: OutputStream) = f

    def inStream(f: OutputStream) = null

    def alive(f: OutputStream) = true
  }

  implicit object InputStreamExpectable extends Expectable[InputStream] {
    def outStream(f: InputStream) = null

    def inStream(f: InputStream) = f

    def alive(f: InputStream) = true
  }
}
