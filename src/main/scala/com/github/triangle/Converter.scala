package com.github.triangle

import java.text.{Format, ParsePosition, NumberFormat}

/**
 * A converter from one type to another.
 * @author Eric Pabst (epabst@gmail.com)
 * Date: 9/15/11
 * Time: 7:40 PM
 */

trait Converter[-A,-B] {
  /**
   * Converts from <code>from</code> to the new type if possible.
   */
  def convertTo[T <: B](from: A)(implicit manifest: Manifest[T]): Option[T]
}

protected abstract class SimpleConverter[-A,-B] extends Converter[A,B] {
  def attemptConvertTo[T <: B](from: A)(implicit manifest: Manifest[T]): T

  def convertTo[T <: B](from: A)(implicit manifest: Manifest[T]) =
    try { Some(attemptConvertTo[T](from)) }
    catch { case e: IllegalArgumentException => None }
}

trait DirectConverter[-A,B] extends Converter[A,B] {
  /**
   * Converts from <code>from</code> to the new type if possible.
   */
  def convert(from: A): Option[B]

  def convertTo[T <: B](from: A)(implicit manifest: Manifest[T]) = convert(from).map(_.asInstanceOf[T])
}

protected abstract class SimpleDirectConverter[-A,B] extends DirectConverter[A,B] {
  def attemptConvert(from: A): B

  def convert(from: A) = try { Some(attemptConvert(from)) } catch { case e: IllegalArgumentException => None }
}

class ParseFormatConverter[T](format: Format, obj2Value: (Object) => T = {(v: Object) => v.asInstanceOf[T]}) extends DirectConverter[String,T] {
  def convert(string: String) = {
    val position = new ParsePosition(0)
    val result = format.parseObject(string, position)
    if (position.getIndex == 0) None else Some(obj2Value(result))
  }
}


class CompositeDirectConverter[-A,B](converters: List[DirectConverter[A,B]]) extends DirectConverter[A,B] {
  def convert(from: A) = converters.view.flatMap(_.convert(from)).headOption
}

object Converter {
  val anyToString: DirectConverter[Any,String] = new DirectConverter[Any,String] {
    def convert(from: Any) = Some(from.toString)
  }

  val stringToAnyVal: Converter[String,AnyVal] = new SimpleConverter[String,AnyVal] {
    def attemptConvertTo[T <: AnyVal](from: String)(implicit manifest: Manifest[T]) = {
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

  private lazy val currencyFormat = NumberFormat.getCurrencyInstance
  private lazy val currencyEditFormat = {
    val editFormat = NumberFormat.getNumberInstance
    editFormat.setMinimumFractionDigits(currencyFormat.getMinimumFractionDigits)
    editFormat.setMaximumFractionDigits(currencyFormat.getMaximumFractionDigits)
    editFormat
  }

  val stringToCurrency: DirectConverter[String,Double] = new CompositeDirectConverter[String,Double](
    List(currencyEditFormat, currencyFormat, NumberFormat.getNumberInstance).map(
      new ParseFormatConverter[Double](_, _.asInstanceOf[Number].doubleValue))
  )

  lazy val currencyToString: DirectConverter[Double,String] = new DirectConverter[Double,String] {
    def convert(from: Double) = Some(currencyFormat.format(from))
  }

  lazy val currencyToEditString: DirectConverter[Double,String] = new DirectConverter[Double,String] {
    def convert(from: Double) = Some(currencyEditFormat.format(from))
  }

  def stringToEnum(enumeration: Enumeration): Converter[String,Enumeration#Value] = new Converter[String,Enumeration#Value] {
    def convertTo[T <: Enumeration#Value](from: String)(implicit manifest: Manifest[T]) =
      enumeration.values.find(_.toString == from).map(_.asInstanceOf[T])
  }
}