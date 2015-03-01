package org.excala

import java.io._

/**
 * Created by Edmund on 2015-01-28.
 */
trait Expectable[-F] {
  def inStream(f: F): InputStream

  def outStream(f: F): OutputStream

  def alive(f: F): Boolean
}