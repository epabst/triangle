package com.github.triangle

trait NoSetter[T] extends PortableField[T] {
  def setter = PortableField.emptyPartialFunction
}

private[triangle] trait NoTransformer[T] extends NoSetter[T] {
  def transformer[S <: AnyRef] = PortableField.emptyPartialFunction
}

trait Transformer[T] extends NoGetter[T] with NoSetter[T]

object Transformer {
  def apply[T](body: PartialFunction[AnyRef,Option[T] => AnyRef]): Transformer[T] = new Transformer[T] {
    def transformer[S <: AnyRef]: PartialFunction[S,Option[T] => S] = {
      case subject: S if body.isDefinedAt(subject) => value => body(subject)(value).asInstanceOf[S]
    }
  }

  /**
   * {@PortableField} support for transforming a subject using a value if {{{subject}}} is of type S.
   * @param S the Subject type to transform using the value
   */
  def apply[S <: AnyRef,T](body: S => Option[T] => S)(implicit _subjectManifest: ClassManifest[S]): PortableField[T] =
    new Transformer[T] with SubjectField {
      def transformer[S1]: PartialFunction[S1,Option[T] => S1] = {
        case subject: S if subjectManifest.erasure.isInstance(subject) => value =>
          body(subject)(value).asInstanceOf[S1]
      }

      def subjectManifest = _subjectManifest

      override def toString = "transformer[" + subjectManifest.erasure.getSimpleName + "]"
    }

  /**
   * {@PortableField} support for transforming a subject using a value if {{{subject}}} is of type S.
   * theTransform operates on a value directly, rather than on an Option.
   * The clearer is used when the value is None.
   * @param S the Subject type to transform using the value
   * @param clearer a function or 'noSetterForEmpty'
   */
  def apply[S <: AnyRef,T](body: S => T => S, clearer: S => S)(implicit subjectManifest: ClassManifest[S]): PortableField[T] =
    Transformer((subject: S) => { (valueOpt: Option[T]) =>
      valueOpt match {
        case Some(value: T) => body(subject)(value).asInstanceOf[S]
        case None => clearer(subject).asInstanceOf[S]
      }
    })
}
