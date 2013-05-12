package com.github.triangle

/** [[com.github.triangle.PortableField]] support for getting a value as an Option if {{{subject}}} is of type S.
  * T is the value type.
  * S is the the Readable type to get the value out of.
  */
abstract class TargetedGetter[S <: AnyRef,T](implicit val subjectManifest: ClassManifest[S]) extends TargetedField[S,T] with Getter[T] with Logging {
  /** An abstract method that must be implemented by subtypes. */
  def get(subject: S): Option[T]

  val singleGetter: PartialFunction[AnyRef,Option[T]] = {
    case subject if isExpectedType(subject) =>
      get(subject.asInstanceOf[S])
  }

  private def isExpectedType(subject: AnyRef): Boolean = {
    val result = subjectManifest.erasure.isInstance(subject)
    result
  }

  /**
   * PartialFunction for getting an optional value from the first AnyRef in the GetterInput that has Some value using getter.
   * If none of them has Some value, then it will return None if at least one of them applies.
   * If none of them even apply, the PartialFunction won't match at all (i.e. isDefinedAt will be false).
   */
  override def getter = {
    case input: GetterInput if input.items.exists(singleGetter.isDefinedAt(_)) =>
      input.items.view.collect(singleGetter).find(_.isDefined).getOrElse(None)
  }
}

trait NoGetter[T] extends PortableField[T] {
  def getter = PortableField.emptyPartialFunction
}

trait Getter[T] extends PortableField[T] {
  def updater[S <: AnyRef] = PortableField.emptyPartialFunction
}

private class Getter2[T](getter: PartialFunction[GetterInput,Option[T]])
    extends SimplePortableField(getter, PortableField.emptyPartialFunction) with Getter[T]

class SingleGetter[T](val singleGetter: PartialFunction[AnyRef,Option[T]])
    extends Getter2[T]({
      case input: GetterInput if input.items.exists(singleGetter.isDefinedAt(_)) =>
        input.items.view.collect(singleGetter).find(_.isDefined).getOrElse(None)
    })

object Getter {
  def apply[T](body: PartialFunction[GetterInput,Option[T]]): Getter[T] = new Getter2[T](body)

  def single[T](body: PartialFunction[AnyRef,Option[T]]): Getter[T] = new Getter2[T]({
    case input: GetterInput if input.items.exists(body.isDefinedAt(_)) =>
      input.items.view.collect(body).find(_.isDefined).getOrElse(None)
  })

  /** Defines a getter field for a type. */
  def apply[S <: AnyRef,T](body: S => Option[T])(implicit subjectManifest: ClassManifest[S]): TargetedGetter[S,T] =
    new TargetedGetter[S,T] {
      def get(subject: S) = body(subject)

      override val toString = "Getter[" + subjectManifest.erasure.getSimpleName + "]"
    }
}
