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
@deprecated("use Field")
abstract class DelegatingPortableField[T] extends FieldWithDelegate[T] {
  protected def delegate: PortableField[T]

  override def getter = delegate.getterVal

  def updater[S <: AnyRef]: PartialFunction[UpdaterInput[S,T],S] = delegate.updaterVal
}

/**
 * a PortableField[T] that wraps another.
 * This is important for creating Field objects or extractors and is also
 * useful when wanting subclasses with additional fields or behavior.
 */
class Field[T](val delegate: PortableField[T]) extends SimplePortableField[T](delegate.getter, delegate.updater) with FieldWithDelegate[T] {
  override lazy val toString = delegate.toString
}

/** A PortableField that delegates to another field that modifies part of the subject, as defined by {{{subjectGetter}}}. */
//todo rename to NestedField and make subjectGetter be a PortableField
trait PartialDelegatingField[T] extends FieldWithDelegate[T] with UpdaterUsingSetter[T] {
  protected def delegate: PortableField[T]
  protected def subjectGetter: PartialFunction[AnyRef,AnyRef]

  private lazy val delegateGetter = delegate.getterVal

  def getter: PartialFunction[GetterInput,Option[T]] = {
    case input: GetterInput if delegateGetter.isDefinedAt(GetterInput(input.items.collect(subjectGetter))) =>
      delegateGetter.apply(GetterInput(input.items.collect(subjectGetter)))
  }

  /** A setter.  It is identical to updater but doesn't have to return the modified subject. */
  def setter[S <: AnyRef]: PartialFunction[UpdaterInput[S,T],Unit] = {
    case input @ UpdaterInput(subject, valueOpt, context)
      if subjectGetter.isDefinedAt(subject) && delegate.updaterVal.isDefinedAt(input.copy(subject = subjectGetter(subject))) =>
        delegate.updaterVal(input.copy(subject = subjectGetter(subject)))
  }
}
