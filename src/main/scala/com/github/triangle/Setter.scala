package com.github.triangle

trait UpdaterUsingSetter[T] extends PortableField[T] {
  /** A setter.  It is identical to updater but doesn't have to return the modified subject. */
  def setter[S <: AnyRef]: PartialFunction[UpdaterInput[S,T],Unit]

  def updater[S <: AnyRef]: PartialFunction[UpdaterInput[S,T],S] = {
    case input @ UpdaterInput(subject, valueOpt, context) if setter.isDefinedAt(input)=>
      setter(input); subject
  }
}

trait Setter[T] extends NoGetter[T] with UpdaterUsingSetter[T]

/** [[com.github.triangle.PortableField]] support for setting a value if {{{subject}}} is of type S.
  * This is a trait so that it can be mixed with FieldGetter.
  * S is the Writable type to put the value into
  */
trait FieldSetter[S <: AnyRef,T] extends Setter[T] with FieldWithSubject[S,T] with Logging {
  def subjectManifest: ClassManifest[S]

  /** An abstract method that must be implemented by subtypes. */
  def set(subject: S, value: Option[T], context: GetterInput)

  def setter[S1 <: AnyRef]: PartialFunction[UpdaterInput[S1,T],Unit] = {
    case UpdaterInput(subject, valueOpt, context) if subjectManifest.erasure.isInstance(subject) =>
     set(subject.asInstanceOf[S], valueOpt, context)
  }
}

object Setter {
  def apply[T](body: PartialFunction[UpdaterInput[AnyRef,T],Unit]): Setter[T] = new Setter[T] {

    /** A setter.  It is identical to updater but doesn't have to return the modified subject. */
    def setter[S <: AnyRef]: PartialFunction[UpdaterInput[S,T],Unit] = {
      case input if body.isDefinedAt(input) => body(input)
    }
  }

  /** Defines setter field for a mutable type with Option as the value type. */
  def apply[S <: AnyRef,T](body: S => Option[T] => Unit)(implicit _subjectManifest: ClassManifest[S]): FieldSetter[S,T] = {
    new FieldSetter[S,T] {
      def subjectManifest = _subjectManifest

      def set(subject: S, valueOpt: Option[T], context: GetterInput) {
        body(subject)(valueOpt)
      }

      override def toString = "setter[" + subjectManifest.erasure.getSimpleName + "]"
    }
  }

  /** Defines a setter field for a Writable type, with separate functions for Some and None.
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
