package com.github.triangle

/** A converter from one type to another.
  * @author Eric Pabst (epabst@gmail.com)
  */
trait GenericConverter[-A,-B] {
  /** Converts from {{{from}}} to the new type if possible. */
  def convertTo[T <: B](from: A)(implicit manifest: Manifest[T]): Option[T]
}

protected abstract class SimpleGenericConverter[-A,-B] extends GenericConverter[A,B] {
  def attemptConvertTo[T <: B](from: A)(implicit manifest: Manifest[T]): T

  def convertTo[T <: B](from: A)(implicit manifest: Manifest[T]) =
    try { Some(attemptConvertTo[T](from)) }
    catch { case e: IllegalArgumentException => None }
}
