package com.github.triangle

@deprecated("use NoUpdater")
private[triangle] trait NoTransformer[T] extends PortableField[T] {
  def updater[S <: AnyRef] = PortableField.emptyPartialFunction
}

@deprecated("use Updater")
trait Transformer[T] extends NoGetter[T]

@deprecated("use Updater or SubjectUpdater")
object Transformer {
  def apply[T](body: PartialFunction[AnyRef,Option[T] => AnyRef]): Transformer[T] = new Transformer[T] {
    def updater[S <: AnyRef]: PartialFunction[UpdaterInput[S,T],S] = {
      case UpdaterInput(subject, valueOpt, context) if body.isDefinedAt(subject)=>
        body(subject)(valueOpt).asInstanceOf[S]
    }
  }

  /** PortableField] support for transforming a subject using a value if {{{subject}}} is of type S.
    * Type S is the Subject type to transform using the value.
    */
  def apply[S <: AnyRef,T](body: S => Option[T] => S)(implicit _subjectManifest: ClassManifest[S]): PortableField[T] =
    new Transformer[T] with SubjectField {
      def updater[S1 <: AnyRef]: PartialFunction[UpdaterInput[S1,T],S1] = {
        case UpdaterInput(subject, valueOpt, context) if subjectManifest.erasure.isInstance(subject) =>
          body(subject.asInstanceOf[S])(valueOpt).asInstanceOf[S1]
      }

      def subjectManifest = _subjectManifest

      override def toString = "Transformer[" + subjectManifest.erasure.getSimpleName + "]"
    }

  /** [[com.github.triangle.PortableField]] support for transforming a subject using a value if {{{subject}}} is of type S.
    * theTransform operates on a value directly, rather than on an Option.
    * The clearer is used when the value is None.
    * Type S is the Subject type to transform using the value.
    * @param clearer a function or 'noSetterForEmpty'
    */
  def apply[S <: AnyRef,T](body: S => T => S, clearer: S => S)(implicit subjectManifest: ClassManifest[S]): PortableField[T] =
    Transformer((subject: S) => { (valueOpt: Option[T]) =>
      valueOpt match {
        case Some(value) => body(subject)(value)
        case None => clearer(subject)
      }
    })
}

@deprecated("use Updater")
trait TransformerUsingItems[T] extends Transformer[T] {
  /** An updater that also has access to some items such as context that may be helpful when transforming. */
  def transformerUsingItems[S <: AnyRef]: PartialFunction[(S,GetterInput),Option[T] => S]

  def updater[S <: AnyRef]: PartialFunction[UpdaterInput[S,T],S] = {
    case UpdaterInput(subject, valueOpt, context) if transformerUsingItems.isDefinedAt((subject, context))=>
      transformerUsingItems((subject, context)).apply(valueOpt)
  }
}

@deprecated("use SubjectUpdater")
trait SubjectTransformer[S <: AnyRef,T] extends TransformerUsingItems[T] with FieldWithSubject[S,T]

@deprecated("use Updater or SubjectUpdater")
object TransformerUsingItems {
  def apply[T](body: PartialFunction[(AnyRef,GetterInput),Option[T] => AnyRef]): Transformer[T] = new TransformerUsingItems[T] {
    override def transformerUsingItems[S <: AnyRef]: PartialFunction[(S,GetterInput),Option[T] => S] = {
      case subjectAndItems if body.isDefinedAt(subjectAndItems) => value => body(subjectAndItems)(value).asInstanceOf[S]
    }
  }

  /** Defines Transformer field defined for a given type S with T as the value type. */
  def apply[S <: AnyRef,T](body: (S, GetterInput) => Option[T] => S)(implicit _subjectManifest: ClassManifest[S]): SubjectTransformer[S,T] = {
    new SubjectTransformer[S,T] {
      override def transformerUsingItems[S1]: PartialFunction[(S1,GetterInput),Option[T] => S1] = {
        case subjectAndItems if subjectManifest.erasure.isInstance(subjectAndItems._1) => value =>
          body(subjectAndItems._1.asInstanceOf[S], subjectAndItems._2)(value).asInstanceOf[S1]
      }

      def subjectManifest = _subjectManifest

      override def toString = "TransformerUsingItems[" + subjectManifest.erasure.getSimpleName + "]"
    }
  }
}
