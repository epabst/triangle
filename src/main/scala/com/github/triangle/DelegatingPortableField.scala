package com.github.triangle

import scala.PartialFunction

/** Any PortableField that contains another PortableField should extend this.
  * @see DelegatingPortableField
  */
trait FieldWithDelegate[T] extends PortableField[T] {
  protected def delegate: BaseField

  override def deepCollect[R](f: PartialFunction[BaseField, R]) = f.lift(this).map(List(_)).getOrElse(delegate.deepCollect(f))
}

/** A FieldWithDelegate that delegates directly to its delegate field. */
trait DelegatingPortableField[T] extends FieldWithDelegate[T] {
  protected def delegate: PortableField[T]

  def getter = delegate.getter

  override def getterFromItem = delegate.getterFromItem

  def setter = delegate.setter

  override def setterUsingItems = delegate.setterUsingItems

  def transformer[S <: AnyRef]: PartialFunction[S,Option[T] => S] = delegate.transformer

  override def transformerUsingItems[S <: AnyRef]: PartialFunction[(S,List[AnyRef]),Option[T] => S] = delegate.transformerUsingItems
}

/** a PortableField[T] that wraps another for use with creating field objects.
  * This is important for creating extractors.
  */
class Field[T](val delegate: PortableField[T]) extends DelegatingPortableField[T] {
  override def toString = delegate.toString
}

trait PartialDelegatingField[T] extends FieldWithDelegate[T] with TransformerUsingSetter[T] {
  protected def delegate: PortableField[T]
  protected def subjectGetter: PartialFunction[AnyRef,AnyRef]

  def getter = {
    case subject if subjectGetter.isDefinedAt(subject) &&
      delegate.getter.isDefinedAt(subjectGetter(subject)) => delegate.getter(subjectGetter(subject))
  }

  override def getterFromItem: PartialFunction[List[_],Option[T]] = {
    case items: List[_] if delegate.getterFromItem.isDefinedAt(items.map(_.asInstanceOf[AnyRef]).collect(subjectGetter)) =>
      delegate.getterFromItem(items.map(_.asInstanceOf[AnyRef]).collect(subjectGetter))
  }

  override def setter: PartialFunction[AnyRef,Option[T] => Unit] = {
    case subject if subjectGetter.isDefinedAt(subject) &&
      delegate.setter.isDefinedAt(subjectGetter(subject)) => delegate.setter(subjectGetter(subject))
  }

  override def setterUsingItems = {
    case (subject, items) if subjectGetter.isDefinedAt(subject) &&
      delegate.setterUsingItems.isDefinedAt((subjectGetter(subject), items)) => delegate.setterUsingItems((subjectGetter(subject), items))
  }
}
