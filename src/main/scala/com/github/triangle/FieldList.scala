package com.github.triangle

import scala.collection._

/** A trait that has a list of Fields.  The only requirement is that {{{fields}}} be defined.
  * It has helpful methods that can operate on them.
  * It implements BaseField in order to use copy methods that return a PortableValue which represents a composite value.
  * @author Eric Pabst (epabst@gmail.com)
  */
trait FieldList extends Traversable[BaseField] with BaseField with Logging {

  protected def fields: Traversable[BaseField]

  def foreach[U](f: (BaseField) => U) { fields.foreach(f) }

  def copyFrom(from: AnyRef) = copyFromUsingCopyMethod[AnyRef](f => f.copyFrom(_), from)

  def copyFrom(fromItems: GetterInput) = copyFromUsingCopyMethod[GetterInput](f => f.copyFrom(_), fromItems)

  private def copyFromUsingCopyMethod[A](baseFieldCopyMethod: BaseField => (A => PortableValue), from: A): PortableValue =
    new PortableValueSeq(fields.map(f => baseFieldCopyMethod(f)(from)))

  /** Narrows the FieldList to fields whose updater isDefinedAt the given subject. */
  def copyableTo(subject: AnyRef): FieldList = copyableTo(subject, GetterInput.empty)

  /** Narrows the FieldList to fields whose updater isDefinedAt the given subject. */
  def copyableTo(subject: AnyRef, contextItems: GetterInput): FieldList =
    copyableTo(UpdaterInput(subject, contextItems))

  /** Narrows the FieldList to fields whose updater isDefinedAt the given subject. */
  def copyableTo(updaterInput: UpdaterInput[_ <: AnyRef, Nothing]): FieldList = deepCollect {
    case (field: PortableField[_]) if field.updater.isDefinedAt(updaterInput) => field
  }

  def transform[S <: AnyRef](initial: S, data: AnyRef): S = copyAndTransform(data, initial)

  def copyAndTransform[S <: AnyRef](data: AnyRef, initial: S): S = {
    fields.foldLeft(initial)((subject, field) => field.copyAndTransform(data, subject))
  }

  def copyAndTransformWithItem[S <: AnyRef](dataItems: GetterInput, initial: S): S = {
    fields.foldLeft(initial)((subject, field) => field.copyAndTransformWithItem(dataItems, subject))
  }

  override def deepCollect[B](f: PartialFunction[BaseField, B]) = fields.toList.flatMap(_.deepCollect(f))
}

object FieldList {
  def apply(_fields: BaseField*): FieldList = toFieldList(_fields)

  implicit def toFieldList(list: Traversable[BaseField]): FieldList = new FieldList { def fields = list }
}
