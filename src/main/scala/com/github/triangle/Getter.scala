package com.github.triangle

/** [[com.github.triangle.PortableField]] support for getting a value as an Option if {{{subject}}} is of type S.
  * T is the value type.
  * S is the the Readable type to get the value out of.
  */
abstract class FieldGetter[S <: AnyRef,T](implicit val subjectManifest: ClassManifest[S]) extends FieldWithSubject[S,T] with SingleGetter[T] with Logging {
  /** An abstract method that must be implemented by subtypes. */
  def get(subject: S): Option[T]

  def singleGetter = { case subject if subjectManifest.erasure.isInstance(subject) => get(subject.asInstanceOf[S]) }
}

trait NoGetter[T] extends PortableField[T] {
  def getter = PortableField.emptyPartialFunction
}

trait Getter[T] extends NoTransformer[T]

trait SingleGetter[T] extends Getter[T] {
  /** PartialFunction for getting an optional value from an AnyRef. */
  def singleGetter: PartialFunction[AnyRef,Option[T]]

  /**
   * PartialFunction for getting an optional value from the first AnyRef in the GetterInput that has Some value using getter.
   * If none of them has Some value, then it will return None if at least one of them applies.
   * If none of them even apply, the PartialFunction won't match at all (i.e. isDefinedAt will be false).
   */
  final override def getter = {
    case input: GetterInput if input.items.exists(singleGetter.isDefinedAt(_)) =>
      input.items.view.collect(singleGetter).find(_.isDefined).getOrElse(None)
  }
}

object Getter {

  def single[T](body: PartialFunction[AnyRef,Option[T]]): Getter[T] = new Getter[T] {

    override def getter = {
      case input: GetterInput if input.items.exists(body.isDefinedAt(_)) =>
        input.items.view.collect(body).find(_.isDefined).getOrElse(None)
    }
  }

  /** Defines a getter field for a type. */
  def apply[S <: AnyRef,T](getter1: S => Option[T])(implicit subjectManifest: ClassManifest[S]): FieldGetter[S,T] =
    new FieldGetter[S,T] with Getter[T] {
      def get(subject: S) = getter1(subject)

      override def toString = "getter[" + subjectManifest.erasure.getSimpleName + "]"
    }
}

/** Like Getter, but passes the list of items so that more than one of the Subjects can be used
  * in getting the value.
  */
object GetterFromItem {
  def apply[T](body: PartialFunction[GetterInput,Option[T]]): Getter[T] = new Getter[T] {
    override def getter = body
  }
}
