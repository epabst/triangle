package com.github.triangle

import scala.collection._

/** A trait that has a list of Fields.  The only requirement is that {{{fields}}} be defined.
  * It has helpful methods that can operate on them.
  * It implements BaseField in order to use copy methods that return a PortableValue which represents a composite value.
  * @author Eric Pabst (epabst@gmail.com)
  */
abstract class FieldList extends BaseField with Traversable[BaseField] with Logging {

  protected def fields: Traversable[BaseField]

  def foreach[U](f: (BaseField) => U) { fields.foreach(f) }

  @scala.inline
  final def copyFrom(from: AnyRef) = copyFrom(GetterInput.single(from))

  def copyFrom(input: GetterInput) = new PortableValueSeq(fields.map(f => f.copyFrom(input)))

  /** Narrows the FieldList to fields whose updater isDefinedAt the given subject. */
  @scala.inline
  final def copyableTo(subject: AnyRef): FieldList = copyableTo(subject, GetterInput.empty)

  /** Narrows the FieldList to fields whose updater isDefinedAt the given subject. */
  @scala.inline
  final def copyableTo(subject: AnyRef, context: GetterInput): FieldList =
    copyableTo(UpdaterInput(subject, context))

  /** Narrows the FieldList to fields whose updater isDefinedAt the given subject. */
  def copyableTo(updaterInput: UpdaterInput[_ <: AnyRef, Nothing]): FieldList = FieldList(deepCollect {
    case (field: PortableField[_]) if field.isUpdaterDefinedAt(updaterInput) => field
  })

  def copyAndUpdate[S <: AnyRef](dataItems: GetterInput, initial: S): S = {
    fields.foldLeft(initial)((subject, field) => field.copyAndUpdate(dataItems, subject))
  }

  override def deepCollect[B](f: PartialFunction[BaseField, B]): Seq[B] = fields.toSeq.flatMap(_.deepCollect(f))
}

object FieldList {
  def apply(_fields: BaseField*): FieldList = toFieldList(_fields)

  def apply(list: Traversable[BaseField]): FieldList = new FieldList { val fields = list }

  implicit def toFieldList(list: Traversable[BaseField]): FieldList = FieldList(list)
}
