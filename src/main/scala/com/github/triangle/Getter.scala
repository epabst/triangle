package com.github.triangle

/**
 * {@PortableField} support for getting a value as an Option if {{{subject}}} is of type S.
 * @param T the value type
 * @param S the Readable type to get the value out of
 */
abstract class FieldGetter[S <: AnyRef,T](implicit val subjectManifest: ClassManifest[S]) extends FieldWithSubject[S,T] with Logging {
  /** An abstract method that must be implemented by subtypes. */
  def get(subject: S): Option[T]

  def getter = { case subject: S if subjectManifest.erasure.isInstance(subject) => get(subject) }
}

trait NoGetter[T] extends PortableField[T] {
  def getter = PortableField.emptyPartialFunction
}

trait Getter[T] extends NoTransformer[T]

object Getter {
  def apply[T](body: PartialFunction[AnyRef,Option[T]]): Getter[T] = new Getter[T] {
    def getter = body
  }

  /** Defines a getter field for a type. */
  def apply[S <: AnyRef,T](getter1: S => Option[T])(implicit subjectManifest: ClassManifest[S]): FieldGetter[S,T] =
    new FieldGetter[S,T] with Getter[T] {
      def get(subject: S) = getter1(subject)

      override def toString = "getter[" + subjectManifest.erasure.getSimpleName + "]"
    }
}
