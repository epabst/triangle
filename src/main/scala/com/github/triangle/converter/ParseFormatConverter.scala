package com.github.triangle.converter

import java.text.{ParsePosition, Format}

/**
 * A Converter that uses a [[java.text.Format]] instance.
 * @author Eric Pabst (epabst@gmail.com)
 *         Date: 2/22/13
 *         Time: 7:43 AM
 */
class ParseFormatConverter[T](format: Format, obj2Value: (Object) => T = {(v: Object) => v.asInstanceOf[T]}) extends Converter[String,T] {
  def convert(string: String) = {
    val position = new ParsePosition(0)
    val result = format.parseObject(string, position)
    if (result == null) None else Some(obj2Value(result))
  }
}
