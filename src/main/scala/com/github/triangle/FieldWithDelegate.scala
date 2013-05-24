package com.github.triangle

import scala.PartialFunction

/** Any PortableField that contains another PortableField should extend this.
  * @see Field
  */
trait FieldWithDelegate[T] extends PortableField[T] {
  protected def delegate: BaseField

  override def deepCollect[R](f: PartialFunction[BaseField, R]) = f.lift(this).map(List(_)).getOrElse(delegate.deepCollect(f))
}

/**
 * a PortableField[T] that wraps another.
 * This is important for creating Field objects or extractors and is also
 * useful when wanting subclasses with additional fields or behavior.
 */
class Field[T](val delegate: PortableField[T]) extends SimplePortableField[T](delegate.getter, delegate.updater) with FieldWithDelegate[T] {
  override def toString = delegate.toString
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

/** A PortableField that delegates to another field that modifies part of the subject, as defined by {{{subjectGetter}}}. */
class NestedField[T](nestedSubjectField: PortableField[AnyRef], val nestedField: PortableField[T])
    extends SimplePortableField[T](getter = {
      case input: GetterInput if nestedSubjectField.getter.isDefinedAt(input) &&
          nestedSubjectField(input).map(nestedSubject => nestedField.getter.isDefinedAt(nestedSubject +: input)).getOrElse(true) =>
        nestedSubjectField.getter(input).flatMap(nestedSubject => nestedField.getter.apply(nestedSubject +: input))
    },
    _updater = {
      case input @ UpdaterInput(subject, valueOpt, context) if nestedSubjectField.getter.isDefinedAt(input.asGetterInput) &&
          nestedSubjectField(input.asGetterInput).map(nestedSubject => nestedField.isUpdaterDefinedAt(input.copy(subject = nestedSubject))).getOrElse(true) =>
        nestedSubjectField(input.asGetterInput).map { nestedSubject =>
          val updatedNestedSubject = nestedField.updaterVal(input.copy(subject = nestedSubject))
          nestedSubjectField.update(input.withValue(Some(updatedNestedSubject)))
        }.getOrElse(subject)
    }) {
  override def deepCollect[R](f: PartialFunction[BaseField, R]) =
    f.lift(this).map(List(_)).getOrElse(nestedSubjectField.deepCollect(f) ++ nestedField.deepCollect(f))
}
