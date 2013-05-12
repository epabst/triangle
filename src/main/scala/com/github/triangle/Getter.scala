package com.github.triangle

/** [[com.github.triangle.PortableField]] support for getting a value as an Option if {{{subject}}} is of type S.
  * T is the value type.
  * S is the the Readable type to get the value out of.
  */
class TargetedGetter[S <: AnyRef,T](_getter: S => Option[T])(implicit val subjectManifest: ClassManifest[S])
    extends SingleGetter[T]({
      case subject if subjectManifest.erasure.isInstance(subject) =>
        _getter(subject.asInstanceOf[S])
    }) with TargetedField[S,T]

class Getter[T](getter: PartialFunction[GetterInput,Option[T]])
    extends SimplePortableField(getter, PortableField.emptyPartialFunction)

class SingleGetter[T](val singleGetter: PartialFunction[AnyRef,Option[T]])
    extends Getter[T]({
      case input: GetterInput if input.items.exists(singleGetter.isDefinedAt(_)) =>
        input.items.view.collect(singleGetter).find(_.isDefined).getOrElse(None)
    })

object Getter {
  def apply[T](body: PartialFunction[GetterInput,Option[T]]): Getter[T] = new Getter[T](body)

  def single[T](body: PartialFunction[AnyRef,Option[T]]): Getter[T] = new SingleGetter[T](body)

  /** Defines a getter field for a type. */
  def apply[S <: AnyRef,T](body: S => Option[T])(implicit subjectManifest: ClassManifest[S]): TargetedGetter[S,T] =
    new TargetedGetter[S,T](subject => body(subject)) {
      override val toString = "Getter[" + subjectManifest.erasure.getSimpleName + "]"
    }
}
