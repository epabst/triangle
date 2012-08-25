package com.github.triangle

/** A PortableField with no updater. */
trait NoUpdater[T] extends PortableField[T] {
  def updater[S <: AnyRef] = PortableField.emptyPartialFunction
}

/** A PortableField that has an updater but no getter. */
trait Updater[T] extends NoGetter[T]

/** An Updater that only applies if the subject is a given type. */
trait TargetedUpdater[S <: AnyRef, T] extends Updater[T] with TargetedField[S, T]

object Updater {
  /** Factory method that takes the body of the updater as a parameter. */
  def apply[T](body: PartialFunction[UpdaterInput[AnyRef,T],AnyRef]): Updater[T] = new Updater[T] {
    def updater[S <: AnyRef]: PartialFunction[UpdaterInput[S, T], S] = {
      case input if body.isDefinedAt(input.asInstanceOf[UpdaterInput[AnyRef,T]]) =>
        body(input.asInstanceOf[UpdaterInput[AnyRef,T]]).asInstanceOf[S]
    }
  }

  /**
   * [[com.github.triangle.PortableField]] support for updating a subject using a value if {{{subject}}} is of type S.
   * It's called "single" because it doesn't use a GetterInput for context.
   * @tparam S the Subject type to update using the value.
   * @tparam T the type of the value
   */
  def apply[S <: AnyRef, T](body: S => Option[T] => S)(implicit _subjectManifest: ClassManifest[S]): TargetedUpdater[S,T] =
    new TargetedUpdater[S,T] {
      def updater[S1 <: AnyRef]: PartialFunction[UpdaterInput[S1, T], S1] = {
        case UpdaterInput(subject, valueOpt, context) if subjectManifest.erasure.isInstance(subject) =>
          body(subject.asInstanceOf[S])(valueOpt).asInstanceOf[S1]
      }

      def subjectManifest = _subjectManifest

      override def toString = "Updater[" + subjectManifest.erasure.getSimpleName + "]"
    }

  /**
   * [[com.github.triangle.PortableField]] support for updating a subject using a value if {{{subject}}} is of type S.
   * body operates on a value directly, rather than on an Option.
   * The clearer is used when the value is None.
   * @tparam S the Subject type to update using the value.
   * @param clearer a function or 'noUpdaterForEmpty'
   */
  def apply[S <: AnyRef, T](body: S => T => S, clearer: S => S)(implicit subjectManifest: ClassManifest[S]): PortableField[T] =
    Updater((subject: S) => _.map(body(subject)).getOrElse(clearer(subject)))
}
