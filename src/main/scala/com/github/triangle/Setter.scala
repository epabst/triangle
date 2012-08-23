package com.github.triangle

trait TransformerUsingSetter[T] extends PortableField[T] {
  def updater[S <: AnyRef]: PartialFunction[UpdaterInput[S,T],S] = {
    case UpdaterInput(subject, valueOpt, context) if setter.isDefinedAt(subject)=>
      setter(subject)(valueOpt); subject
  }

  override def transformerUsingItems[S <: AnyRef]: PartialFunction[(S,GetterInput),Option[T] => S] = {
    case subjectAndItems if setterUsingItems.isDefinedAt(subjectAndItems) => { value =>
      setterUsingItems(subjectAndItems).apply(value); subjectAndItems._1
    }
  }
}

trait Setter[T] extends NoGetter[T] with TransformerUsingSetter[T]

trait SetterUsingItems[T] extends Setter[T] {
  override def setterUsingItems: PartialFunction[(AnyRef,GetterInput),Option[T] => Unit]

  def setter = {
    case context if setterUsingItems.isDefinedAt((context, GetterInput.empty)) => setterUsingItems((context, GetterInput.empty))
  }
}

/** [[com.github.triangle.PortableField]] support for setting a value if {{{subject}}} is of type S.
  * This is a trait so that it can be mixed with FieldGetter.
  * S is the Writable type to put the value into
  */
trait FieldSetter[S <: AnyRef,T] extends SetterUsingItems[T] with FieldWithSubject[S,T] with TransformerUsingSetter[T] with Logging {
  def subjectManifest: ClassManifest[S]

  /** An abstract method that must be implemented by subtypes. */
  @deprecated("use GetterInput instead of List[AnyRef]")
  def set(subject: S, value: Option[T], items: List[AnyRef])

  def set(subject: S, value: Option[T], items: GetterInput) {
    set(subject, value, items.items.toList)
  }

  override def setterUsingItems = {
    case (subject, input) if subjectManifest.erasure.isInstance(subject) => set(subject.asInstanceOf[S], _, input)
  }
}

object Setter {
  def apply[T](body: PartialFunction[AnyRef,Option[T] => Unit]): Setter[T] = new Setter[T] {
    def setter = body
  }

  /** Defines setter field for a mutable type with Option as the value type. */
  def apply[S <: AnyRef,T](body: S => Option[T] => Unit)(implicit _subjectManifest: ClassManifest[S]): FieldSetter[S,T] = {
    new FieldSetter[S,T] {
      def subjectManifest = _subjectManifest

      def set(subject: S, valueOpt: Option[T], items: List[AnyRef]) {
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

object SetterUsingItems {
  def apply[T](body: PartialFunction[(AnyRef,GetterInput),Option[T] => Unit]): Setter[T] = new SetterUsingItems[T] {
    override def setterUsingItems = body
  }

  /** Defines setter field for a mutable type with Option as the value type. */
  def apply[S <: AnyRef,T](body: (S, GetterInput) => Option[T] => Unit)(implicit _subjectManifest: ClassManifest[S]): FieldSetter[S,T] = {
    new FieldSetter[S,T] with NoGetter[T] {
      def subjectManifest = _subjectManifest

      def set(subject: S, valueOpt: Option[T], items: List[AnyRef]) {
        body(subject, GetterInput(items))(valueOpt)
      }

      override def toString = "SetterUsingItems[" + subjectManifest.erasure.getSimpleName + "]"
    }
  }
}
