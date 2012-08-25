package com.github.triangle

/**
 * The input to a [[com.github.triangle.PortableField]] updater.
 * Usage in updater: case UpdaterInput(subject, valueOpt, ...see usage of GetterInput...) =>
 * @param subject the primary subject that will be updated
 * @param valueOpt the value to put into the subject
 * @param context any context items (which may or may not include the primary subject) that may be useful.
 * @author Eric Pabst (epabst@gmail.com)
 *         Date: 8/16/12
 *         Time: 8:41 AM
 */
case class UpdaterInput[+S <: AnyRef,+T](subject: S, valueOpt: Option[T], context: GetterInput = GetterInput.empty) {
  def withValue[T2](newValueOpt: Option[T2]): UpdaterInput[S,T2] = copy(valueOpt = newValueOpt)

  lazy val withUndeterminedValue = withValue(UpdaterInput.undeterminedValue)
}

object UpdaterInput {
  def apply[S <: AnyRef](subject: S, context: GetterInput): UpdaterInput[S,Nothing] =
    UpdaterInput(subject, undeterminedValue, context)

  def undeterminedValue: Option[Nothing] = None
}
