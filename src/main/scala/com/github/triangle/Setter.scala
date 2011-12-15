package com.github.triangle

trait NoSetter[T] extends PortableField[T] {
  def setter = PortableField.emptyPartialFunction
}

/**
 * {@PortableField} support for setting a value if {{{subject}}} is of type S.
 * This is a trait so that it can be mixed with FieldGetter.
 * @param S the Writable type to put the value into
 */
trait FieldSetter[S <: AnyRef,T] extends FieldWithSubject[S,T] with TransformerUsingSetter[T] with Logging {
  def subjectManifest: ClassManifest[S]

  /** An abstract method that must be implemented by subtypes. */
  def set(subject: S, value: Option[T])

  def setter = {
    case subject: S if subjectManifest.erasure.isInstance(subject) => set(subject, _)
  }
}

trait Setter[T] extends NoGetter[T] with TransformerUsingSetter[T]

object Setter {
  def apply[T](body: PartialFunction[AnyRef,Option[T] => Unit]): Setter[T] = new Setter[T] {
    def setter = body
  }

  /** Defines setter field for a mutable type with Option as the value type. */
  def apply[S <: AnyRef,T](body: S => Option[T] => Unit)(implicit _subjectManifest: ClassManifest[S]): FieldSetter[S,T] = {
    new FieldSetter[S,T] {
      def subjectManifest = _subjectManifest

      def set(subject: S, valueOpt: Option[T]) {
        body(subject)(valueOpt)
      }

      def getter = PortableField.emptyPartialFunction

      override def toString = "setter[" + subjectManifest.erasure.getSimpleName + "]"
    }
  }

  /**
   * Defines a setter field for a Writable type, with separate functions for Some and None.
   * The body operates on a value directly, rather than on an Option.
   * The clearer is used when the value is None.
   * @param clearer a function or 'noSetterForEmpty'
   */
  def apply[S <: AnyRef,T](body: S => T => Unit, clearer: S => Unit)
                          (implicit subjectManifest: ClassManifest[S]): FieldSetter[S,T] =
    apply[S,T](fromDirect(body, clearer))

  private def fromDirect[S,T](setter: S => T => Unit, clearer: S => Unit = {_: S => })
                             (implicit subjectManifest: ClassManifest[S]): S => Option[T] => Unit = {
    subject => { valueOpt =>
      valueOpt match {
        case Some(value) => setter(subject)(value)
        case None => clearer(subject)
      }
    }
  }
}
