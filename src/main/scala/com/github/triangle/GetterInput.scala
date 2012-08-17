package com.github.triangle

/**
 * A sequence of input subjects (or sources) for getting [[com.github.triangle.PortableField]] values out of.
 * The first one is the primary subject and the rest are there as "context".
 * Usage in getter: case GetterInput(elems) => ...
 * It's purpose is to make it not ambiguous between wanting to use a Seq as one of the items and having the input Seq.
 * @author Eric Pabst (epabst@gmail.com)
 *         Date: 8/16/12
 *         Time: 8:42 AM
 */

case class GetterInput(items: Seq[AnyRef]) {
  def +:(item: AnyRef): GetterInput = GetterInput(item +: items)
}

/** Usage: GetterInput(foo, bar, baz) and pass to PortableField.getter */
object GetterInput {
  val empty = GetterInput(Nil)

  def single(item: AnyRef): GetterInput = GetterInput(Seq(item))

  def apply(item0: AnyRef, item1: AnyRef, items: AnyRef*): GetterInput = new GetterInput(item0 +: item1 +: items.toList)
}
