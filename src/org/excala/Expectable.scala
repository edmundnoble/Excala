package org.excala

import java.io._

trait Expectable[-F] {
  def inStream(f: F): InputStream

  def outStream(f: F): OutputStream

  def alive(f: F): Boolean
}