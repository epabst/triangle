package com.github.triangle

/** A value that a PortableField operates on.
  * @author Eric Pabst (epabst@gmail.com)
  */
trait PortableValue {
  /**
   * Copies this value to {{{to}}}, if possible.
   */
  def copyTo(to: AnyRef)

  /**
   * Copies this value to {{{to}}} without seeing if the setter isDefinedAt that {{{to}}}.
   */
  protected[triangle] def copyToDefinedAt(to: AnyRef)

  /**
   * Transforms the {{{initial}}} subject using this value.
   * @return the transformed subject, which could be the initial instance
   */
  def transform[S <: AnyRef](initial: S): S

  /** Returns the value contained in this value for the given PortableField. */
  def get[T](field: PortableField[T]): Option[T]
}
