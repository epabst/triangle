package com.github.triangle

/** A PortableField that has an updater but no getter. */
class Updater[T](updater: PartialFunction[UpdaterInput[AnyRef,T],AnyRef])
    extends SimplePortableField[T](PortableField.emptyPartialFunction, updater)

/** An Updater that only applies if the subject is a given type. */
class TargetedUpdater[S <: AnyRef, T](updater: PartialFunction[UpdaterInput[S,T],S])(implicit val subjectManifest: ClassManifest[S])
    extends SimplePortableField[T](PortableField.emptyPartialFunction,
      updater.asInstanceOf[PartialFunction[UpdaterInput[AnyRef,T],AnyRef]]) with TargetedField[S, T]

object Updater {
  /** Factory method that takes the body of the updater as a parameter. */
  def apply[T](body: PartialFunction[UpdaterInput[AnyRef,T],AnyRef]): Updater[T] = new Updater[T](body)

  /**
   * [[com.github.triangle.PortableField]] support for updating a subject using a value if {{{subject}}} is of type S.
   * It's called "single" because it doesn't use a GetterInput for context.
   * @tparam S the Subject type to update using the value.
   * @tparam T the type of the value
   */
  def apply[S <: AnyRef, T](body: S => Option[T] => S)(implicit subjectManifest: ClassManifest[S]): TargetedUpdater[S,T] =
    new TargetedUpdater[S,T]({
      case UpdaterInput(subject, valueOpt, context) if subjectManifest.erasure.isInstance(subject) =>
        body(subject)(valueOpt)
    }) {
      override val toString = "Updater[" + subjectManifest.erasure.getSimpleName + "]"
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
