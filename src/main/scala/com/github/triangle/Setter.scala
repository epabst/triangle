package com.github.triangle

import scala.PartialFunction

trait UpdaterUsingSetter[T] extends PortableField[T] {
  /** A setter.  It is identical to updater but doesn't have to return the modified subject. */
  def setter[S <: AnyRef]: PartialFunction[UpdaterInput[S,T],Unit]

  def updater[S <: AnyRef]: PartialFunction[UpdaterInput[S,T],S] = {
    case input if setter.isDefinedAt(input)=>
      setter(input); input.subject
  }
}

class UpdaterUsingSetter2[T](getter: PartialFunction[GetterInput,Option[T]], _setter: PartialFunction[UpdaterInput[AnyRef,T],Unit])
    extends SimplePortableField[T](getter, SimplePortableField.asUpdaterFunction {
      case input @ UpdaterInput(subject, valueOpt, context) if _setter.isDefinedAt(input)=>
        _setter(input); subject
    }) {
  def this(setter: PartialFunction[UpdaterInput[AnyRef,T],Unit]) {
    this(PortableField.emptyPartialFunction, setter)
  }

  /** A setter.  It is identical to updater but doesn't have to return the modified subject. */
  def setter[S <: AnyRef] = _setter.asInstanceOf[PartialFunction[UpdaterInput[S,T],Unit]]
}

/** [[com.github.triangle.PortableField]] support for setting a value if {{{subject}}} is of type S.
  * S is the Writable type to put the value into
  */
class TargetedSetter[S <: AnyRef,T](setter: PartialFunction[UpdaterInput[S,T],Unit])
                                   (implicit val subjectManifest: ClassManifest[S])
    extends Setter[T](setter.asInstanceOf[PartialFunction[UpdaterInput[AnyRef,T],Unit]]) with TargetedField[S,T]

class Setter[T](_setter: PartialFunction[UpdaterInput[AnyRef,T],Unit])
    extends SimplePortableField[T](PortableField.emptyPartialFunction, SimplePortableField.asUpdaterFunction {
      case input @ UpdaterInput(subject, valueOpt, context) if _setter.isDefinedAt(input)=>
        _setter(input); subject
    }) {
  /** A setter.  It is identical to updater but doesn't have to return the modified subject. */
  def setter[S <: AnyRef] = _setter.asInstanceOf[PartialFunction[UpdaterInput[S,T],Unit]]
}

object Setter {
  def apply[T](body: PartialFunction[UpdaterInput[AnyRef,T],Unit]): Setter[T] = new Setter[T]({
    case input if body.isDefinedAt(input) => body(input)
  })

  /** Defines setter field for a mutable type with Option as the value type. */
  def apply[S <: AnyRef,T](body: S => Option[T] => Unit)(implicit subjectManifest: ClassManifest[S]): TargetedSetter[S,T] =
    new TargetedSetter[S,T](setter = {
      case UpdaterInput(subject, valueOpt, context) if subjectManifest.erasure.isInstance(subject) =>
        body(subject.asInstanceOf[S])(valueOpt)
    })(subjectManifest) {
      override val toString = "Setter[" + subjectManifest.erasure.getSimpleName + "]"
    }

  /** Defines a setter field for a Writable type, with separate functions for Some and None.
    * The body operates on a value directly, rather than on an Option.
    * The clearer is used when the value is None.
    * @param clearer a function or 'noSetterForEmpty'
    */
  def apply[S <: AnyRef,T](body: S => T => Unit, clearer: S => Unit)
                          (implicit subjectManifest: ClassManifest[S]): TargetedSetter[S,T] =
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
