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
  private lazy val subjectGetterFunct = PartialFunct(subjectGetter)

  private lazy val delegateGetter = delegate.getterVal

  def getter: PartialFunct[GetterInput,Option[T]] = new PartialFunct[GetterInput, Option[T]] {
    def isDefinedAt(input: GetterInput) = delegateGetter.isDefinedAt(GetterInput(input.items.collect(subjectGetterFunct)))

    def attempt(input: GetterInput) = delegateGetter.attempt(GetterInput(input.items.collect(subjectGetterFunct)))
  }

  /** A setter.  It is identical to updater but doesn't have to return the modified subject. */
  def setter[S <: AnyRef]: PartialFunct[UpdaterInput[S,T],Unit] = new PartialFunct[UpdaterInput[S, T], Unit] {
    def isDefinedAt(input: UpdaterInput[S, T]) = {
      val subject: S = input.subject
      subjectGetterFunct.attempt(subject).exists { nestedSubject =>
        delegate.updaterVal.isDefinedAt(input.copy(subject = nestedSubject))
      }
    }

    def attempt(input: UpdaterInput[S, T]) = {
      val subject: S = input.subject
      subjectGetterFunct.attempt(subject).map { nestedSubject =>
        delegate.updaterVal.attempt(input.copy(subject = nestedSubject))
        Unit
      }
    }
  }
}

/** A PortableField that delegates to another field that modifies part of the subject, as defined by {{{subjectGetter}}}. */
class NestedField[T](nestedSubjectField: PortableField[AnyRef], val nestedField: PortableField[T])
    extends SimplePortableField[T](getter = new PartialFunct[GetterInput, Option[T]] {
      def isDefinedAt(input: GetterInput) =
          nestedSubjectField.getterVal.attempt(input).exists(nestedSubject => nestedField.getter.isDefinedAt(nestedSubject +: input))

      def attempt(input: GetterInput) =
        nestedSubjectField.getterVal.attempt(input).flatMap(nestedSubject => nestedField.getterVal.attempt(nestedSubject +: input))
    },
    _updater = new PartialFunct[UpdaterInput[AnyRef, T], AnyRef] {
      def isDefinedAt(input: UpdaterInput[AnyRef, T]) =
        nestedSubjectField.getterVal.attempt(input.asGetterInput).exists(nestedSubject => nestedField.updaterVal.isDefinedAt(input.copy(subject = nestedSubject)))

      def attempt(input: UpdaterInput[AnyRef, T]) =
        nestedSubjectField.getterVal.attempt(input.asGetterInput).map { nestedSubject =>
          nestedField.updaterVal.attempt(input.copy(subject = nestedSubject)).map { updatedNestedSubject =>
            nestedSubjectField.update(input.withValue(Some(updatedNestedSubject)))
          }.getOrElse(input.subject)
        }
    }) {
  override def deepCollect[R](f: PartialFunction[BaseField, R]) =
    f.lift(this).map(List(_)).getOrElse(nestedSubjectField.deepCollect(f) ++ nestedField.deepCollect(f))
}
