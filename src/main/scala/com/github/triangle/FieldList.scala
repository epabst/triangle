package com.github.triangle

import scala.collection._

/** A trait that has a list of Fields.  The only requirement is that {{{fields}}} be defined.
  * It has helpful methods that can operate on them.
  * It implements BaseField in order to use copy methods that return a PortableValue which represents a composite value.
  * @author Eric Pabst (epabst@gmail.com)
  */
trait FieldList extends Traversable[BaseField] with BaseField {

  protected def fields: Traversable[BaseField]

  def foreach[U](f: (BaseField) => U) { fields.foreach(f) }

  def copyFrom(from: AnyRef) = copyFromUsingCopyMethod[AnyRef](f => f.copyFrom(_), from)

  def copyFromItem(fromItems: List[AnyRef]) = copyFromUsingCopyMethod[List[AnyRef]](f => f.copyFromItem(_), fromItems)

  private def copyFromUsingCopyMethod[A](baseFieldCopyMethod: BaseField => (A => PortableValue), from: A): PortableValue = {
    val portableValues = fields.map(f => baseFieldCopyMethod(f)(from))
    new PortableValue {
      def copyTo(to: AnyRef, contextItems: List[AnyRef] = Nil) { portableValues.foreach(_.copyTo(to, contextItems)) }

      def transform[S <: AnyRef](initial: S, contextItems: List[AnyRef] = Nil): S = {
        portableValues.foldLeft(initial)((subject, portableValue) => portableValue.transform(subject, contextItems))
      }

      def get[T](field: PortableField[T]): Option[T] = portableValues.view.flatMap(_.get(field)).headOption

      override def toString = "PortableValue(" + portableValues.mkString(",") + ")"
    }
  }

  /** Narrows the FieldList to fields whose transformer isDefinedAt the given subject. */
  def copyableTo(subject: AnyRef): FieldList = deepCollect {
    case (field: PortableField[_]) if field.transformer.isDefinedAt(subject) => field
  }

  def transform[S <: AnyRef](initial: S, data: AnyRef): S = {
    fields.foldLeft(initial)((subject, field) => field.transform(subject, data))
  }

  def transformWithItem[S <: AnyRef](initial: S, dataItems: List[AnyRef]): S = {
    fields.foldLeft(initial)((subject, field) => field.transformWithItem(subject, dataItems))
  }

  override def deepCollect[B](f: PartialFunction[BaseField, B]) = fields.toList.flatMap(_.deepCollect(f))
}

object FieldList {
  def apply(_fields: BaseField*): FieldList = toFieldList(_fields)

  implicit def toFieldList(list: Traversable[BaseField]): FieldList = new FieldList { def fields = list }
}
