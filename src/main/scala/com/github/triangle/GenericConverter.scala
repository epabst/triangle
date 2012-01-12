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

object GenericConverter {
  val stringToAnyVal: GenericConverter[String,AnyVal] = new SimpleGenericConverter[String,AnyVal] {
    def attemptConvertTo[T <: AnyVal](from: String)(implicit manifest: Manifest[T]): T = {
      manifest.erasure match {
        case x: Class[_] if (x == classOf[Int]) => from.toInt.asInstanceOf[T]
        case x: Class[_] if (x == classOf[Long]) => from.toLong.asInstanceOf[T]
        case x: Class[_] if (x == classOf[Short]) => from.toShort.asInstanceOf[T]
        case x: Class[_] if (x == classOf[Byte]) => from.toByte.asInstanceOf[T]
        case x: Class[_] if (x == classOf[Double]) => from.toDouble.asInstanceOf[T]
        case x: Class[_] if (x == classOf[Float]) => from.toFloat.asInstanceOf[T]
        case x: Class[_] if (x == classOf[Boolean]) => from.toBoolean.asInstanceOf[T]
        case _ => throw new IllegalArgumentException("Unknown primitive type: " + classManifest.erasure +
                " with value " + from)
      }
    }
  }
}
