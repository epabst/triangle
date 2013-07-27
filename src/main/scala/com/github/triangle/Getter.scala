package com.github.triangle

/** [[com.github.triangle.PortableField]] support for getting a value as an Option if {{{subject}}} is of type S.
  * T is the value type.
  * S is the the Readable type to get the value out of.
  */
class TargetedGetter[S <: AnyRef,T](_getter: S => Option[T])(implicit val subjectManifest: ClassManifest[S])
    extends SingleGetter[T](new PartialFunct[AnyRef,Option[T]] {
      private val subjectErasure = subjectManifest.erasure

      def isDefinedAt(subject: AnyRef) = subjectErasure.isInstance(subject)

      def attempt(subject: AnyRef) = {
        if (isDefinedAt(subject)) {
          Some(_getter(subject.asInstanceOf[S]))
        } else None
      }
    }) with TargetedField[S,T]

class Getter[T](getter: PartialFunction[GetterInput,Option[T]])
    extends SimplePortableField(getter, PortableField.emptyPartialFunction)

class SingleGetter[T](val singleGetter: PartialFunction[AnyRef,Option[T]])
    extends Getter[T](new PartialFunct[GetterInput, Option[T]] {
      private val singleGetterFunct = PartialFunct(singleGetter)

      def isDefinedAt(input: GetterInput) = input.items.exists(singleGetter.isDefinedAt(_))

      def attempt(input: GetterInput) = attemptUsingItems(input.items)

      private def attemptUsingItems(items: Seq[AnyRef]): Option[Option[T]] = {
        if (items.isEmpty) {
          None
        } else {
          val head = items.head
          singleGetterFunct.attempt(head) match {
            case result @ Some(_: Some[_]) =>
              result
            case result @ Some(None) =>
              attemptUsingItems(items.tail).orElse(result)
            case _ =>
              attemptUsingItems(items.tail)
          }
        }
      }
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
