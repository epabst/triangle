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

  override def getter = delegate.getter

  def updater[S <: AnyRef]: PartialFunction[UpdaterInput[S,T],S] = delegate.updater
}

/** a PortableField[T] that wraps another for use with creating field objects.
  * This is important for creating extractors.
  */
class Field[T](val delegate: PortableField[T]) extends DelegatingPortableField[T] {
  override def toString = delegate.toString
}

/** A PortableField that delegates to another field that modifies part of the subject, as defined by {{{subjectGetter}}}. */
//todo rename to NestedField and make subjectGetter be a PortableField
trait PartialDelegatingField[T] extends FieldWithDelegate[T] with UpdaterUsingSetter[T] {
  protected def delegate: PortableField[T]
  protected def subjectGetter: PartialFunction[AnyRef,AnyRef]

  def getter: PartialFunction[GetterInput,Option[T]] = {
    case input: GetterInput if delegate.getter.isDefinedAt(GetterInput(input.items.collect(subjectGetter))) =>
      delegate.getter(GetterInput(input.items.collect(subjectGetter)))
  }

  /** A setter.  It is identical to updater but doesn't have to return the modified subject. */
  def setter[S <: AnyRef]: PartialFunction[UpdaterInput[S,T],Unit] = {
    case input @ UpdaterInput(subject, valueOpt, context)
      if subjectGetter.isDefinedAt(subject) && delegate.updater.isDefinedAt(input.copy(subject = subjectGetter(subject))) =>
        delegate.updater(input.copy(subject = subjectGetter(subject)))
  }
}
