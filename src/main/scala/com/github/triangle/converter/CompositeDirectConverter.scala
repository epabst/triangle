package com.github.triangle.converter

/**
 * A converter that delegates to any of multiple Converters.
 * @author Eric Pabst (epabst@gmail.com)
 *         Date: 2/22/13
 *         Time: 7:43 AM
 */
class CompositeDirectConverter[-A,+B](converters: List[Converter[A,B]]) extends Converter[A,B] {
  def convert(from: A) = converters.view.flatMap(_.convert(from)).headOption
}
