package com.github.triangle.converter

import java.text.{ParsePosition, Format}

/**
 * A Converter that uses a [[java.text.Format]] ThreadLocal instance.
 * @author Eric Pabst (epabst@gmail.com)
 *         Date: 2/22/13
 *         Time: 7:43 AM
 * @param format a ThreadLocal with a Format.  It's not just a Format because many Formats are not thread-safe.
 *               Make sure that it value is set for all threads using this converter (i.e. its initValue() should fully initialize it).
 */
class ParseFormatConverter[T](format: ThreadLocal[_ <: Format], obj2Value: (Object) => T = {(v: Object) => v.asInstanceOf[T]}) extends Converter[String,T] {
  def convert(string: String) = {
    val position = new ParsePosition(0)
    val result = format.get().parseObject(string, position)
    if (result == null) None else Some(obj2Value(result))
  }
}
