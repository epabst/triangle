package com.github.triangle

import collection.immutable
import collection.immutable.LinearSeq
import collection.generic.GenSeqFactory
import collection.mutable

/**
 * A sequence of input subjects (or sources) for getting [[com.github.triangle.PortableField]] values out of.
 * The first one is the primary subject and the rest are there as "context".
 * Usage in getter: case GetterInput(elems) => ...
 * @author Eric Pabst (epabst@gmail.com)
 *         Date: 8/16/12
 *         Time: 8:42 AM
 */

class GetterInput[+S <: AnyRef](elems: immutable.Seq[S]) extends LinearSeq[S] {

  override def companion = GetterInput

  def apply(idx: Int) = elems(idx)

  def length = elems.length
}

/** Usage: GetterInput(foo, bar, baz) and pass to PortableField.getter */
object GetterInput extends GenSeqFactory[GetterInput] {
  def newBuilder[A <: AnyRef] = new mutable.Builder[A, GetterInput[A]] {
    private val buffer = mutable.Buffer[A]()

    def +=(elem: A) = {
      buffer += elem
      this
    }

    def clear() {
      buffer.clear()
    }

    def result() = new GetterInput(buffer.toList)
  }
}
