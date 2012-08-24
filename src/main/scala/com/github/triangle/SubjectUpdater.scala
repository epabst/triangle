package com.github.triangle

/**
 * An Updater that only applies if the subject is a given type.
 * @author Eric Pabst (epabst@gmail.com)
 *         Date: 8/24/12
 *         Time: 5:11 PM
 */
trait SubjectUpdater[S <: AnyRef, T] extends Updater[T] with FieldWithSubject[S, T]

object SubjectUpdater {
  /**
   * [[com.github.triangle.PortableField]] support for updating a subject using a value if {{{subject}}} is of type S.
   * It's called "single" because it doesn't use a GetterInput for context.
   * @tparam S the Subject type to transform using the value.
   * @tparam T the type of the value
   */
  def apply[S <: AnyRef, T](body: S => Option[T] => S)(implicit _subjectManifest: ClassManifest[S]): SubjectUpdater[S,T] =
    new SubjectUpdater[S,T] {
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
   * @tparam S the Subject type to transform using the value.
   * @param clearer a function or 'noUpdaterForEmpty'
   */
  def apply[S <: AnyRef, T](body: S => T => S, clearer: S => S)(implicit subjectManifest: ClassManifest[S]): PortableField[T] =
    SubjectUpdater((subject: S) => _.map(body(subject)).getOrElse(clearer(subject)))
}
