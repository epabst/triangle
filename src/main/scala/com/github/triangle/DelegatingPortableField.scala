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

  override def getterFromItem = delegate.getterFromItem

  def setter = delegate.setter

  override def setterUsingItems = delegate.setterUsingItems

  def updater[S <: AnyRef]: PartialFunction[UpdaterInput[S,T],S] = delegate.updater

  override def transformerUsingItems[S <: AnyRef]: PartialFunction[(S,GetterInput),Option[T] => S] = delegate.transformerUsingItems
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

  def getterFromItem: PartialFunction[GetterInput,Option[T]] = {
    case input: GetterInput if delegate.getterFromItem.isDefinedAt(GetterInput(input.items.collect(subjectGetter))) =>
      delegate.getterFromItem(GetterInput(input.items.collect(subjectGetter)))
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
