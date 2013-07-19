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
class NestedField[T](nestedSubjectField: PortableField[AnyRef], val nestedField: PortableField[T])
    extends SimplePortableField[T](getter = new PartialFunct[GetterInput, Option[T]] {
      def isDefinedAt(input: GetterInput) =
        nestedSubjectField.getterVal.attempt(input) match {
          case Some(Some(nestedSubject)) =>
            nestedField.getter.isDefinedAt(nestedSubject +: input)
          case Some(None) => true
          case None => false
        }

      def attempt(input: GetterInput) =
        nestedSubjectField.getterVal.attempt(input) match {
          case Some(Some(nestedSubject)) =>
            nestedField.getterVal.attempt(nestedSubject +: input)
          case Some(None) => Some(None)
          case None => None
        }
    },
    _updater = new PartialFunct[UpdaterInput[AnyRef, T], AnyRef] {
      def isDefinedAt(input: UpdaterInput[AnyRef, T]) = {
        nestedSubjectField.getterVal.attempt(input.asGetterInput) match {
          case Some(Some(nestedSubject)) =>
            nestedField.updaterVal.isDefinedAt(input.copy(subject = nestedSubject))
          case Some(None) => true
          case None => false
        }
      }

      def attempt(input: UpdaterInput[AnyRef, T]) =
        nestedSubjectField.getterVal.attempt(input.asGetterInput) match {
          case Some(Some(nestedSubject)) =>
            nestedField.updaterVal.attempt(input.copy(subject = nestedSubject))
          case Some(None) => Some(input.subject)
          case None => None
        }
    }) {
  override def deepCollect[R](f: PartialFunction[BaseField, R]) =
    f.lift(this).map(List(_)).getOrElse(nestedSubjectField.deepCollect(f) ++ nestedField.deepCollect(f))

  override def toString = "NestedField(" + nestedSubjectField +"," + nestedField + ")"
}
