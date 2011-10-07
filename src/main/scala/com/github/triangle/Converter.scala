package com.github.triangle

import java.text.{DateFormat, Format, ParsePosition, NumberFormat}
import java.util.{Calendar, Date}

/**
 * A converter from one type to another.
 * @author Eric Pabst (epabst@gmail.com)
 * Date: 9/15/11
 * Time: 7:40 PM
 */

trait GenericConverter[-A,-B] {
  /**
   * Converts from {{{from}}} to the new type if possible.
   */
  def convertTo[T <: B](from: A)(implicit manifest: Manifest[T]): Option[T]
}

protected abstract class SimpleGenericConverter[-A,-B] extends GenericConverter[A,B] {
  def attemptConvertTo[T <: B](from: A)(implicit manifest: Manifest[T]): T

  def convertTo[T <: B](from: A)(implicit manifest: Manifest[T]) =
    try { Some(attemptConvertTo[T](from)) }
    catch { case e: IllegalArgumentException => None }
}

trait Converter[-A,B] extends GenericConverter[A,B] {
  /**
   * Converts from {{{from}}} to the new type if possible.
   */
  def convert(from: A): Option[B]

  def convertTo[T <: B](from: A)(implicit manifest: Manifest[T]) = convert(from).map(_.asInstanceOf[T])
}

abstract class SimpleConverter[-A,B] extends Converter[A,B] {
  protected def attemptConvert(from: A): B

  def convert(from: A) = try { Some(attemptConvert(from)) } catch { case e: IllegalArgumentException => None }
}

class ParseFormatConverter[T](format: Format, obj2Value: (Object) => T = {(v: Object) => v.asInstanceOf[T]}) extends Converter[String,T] {
  def convert(string: String) = {
    val position = new ParsePosition(0)
    val result = format.parseObject(string, position)
    if (position.getIndex == 0) None else Some(obj2Value(result))
  }
}


class CompositeDirectConverter[-A,B](converters: List[Converter[A,B]]) extends Converter[A,B] {
  def convert(from: A) = converters.view.flatMap(_.convert(from)).headOption
}

object Converter {
  val anyToString: Converter[Any,String] = new Converter[Any,String] {
    def convert(from: Any) = Some(from.toString)
  }

  val stringToAnyVal: GenericConverter[String,AnyVal] = new SimpleGenericConverter[String,AnyVal] {
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

  val stringToCurrency: Converter[String,Double] = new CompositeDirectConverter[String,Double](
    List(currencyEditFormat, currencyFormat, NumberFormat.getNumberInstance).map(
      new ParseFormatConverter[Double](_, _.asInstanceOf[Number].doubleValue))
  )

  def converter[A,B](f: A => Option[B]): Converter[A,B] = new Converter[A,B] {
    def convert(from: A) = f(from)
  }

  lazy val dateToLong = converter[Date, Long](d => Some(d.getTime))
  lazy val longToDate = converter[Long, Date](l => Some(new Date(l)))

  lazy val dateToCalendar = converter[Date, Calendar] { date =>
    val calendar = Calendar.getInstance
    calendar.setTime(date)
    Some(calendar)
  }
  lazy val calendarToDate = converter[Calendar, Date](c => Some(c.getTime))

  def formatToString[T](format: Format): Converter[T,String] = converter[T,String](value => Some(format.format(value)))

  lazy val currencyToString: Converter[Double,String] = formatToString[Double](currencyFormat)
  lazy val currencyToEditString: Converter[Double,String] = formatToString[Double](currencyEditFormat)

  private lazy val dateFormats = List(DateFormat.getDateInstance, new java.text.SimpleDateFormat("MM/dd/yyyy"), new java.text.SimpleDateFormat("dd MMM yyyy"))
  lazy val stringToDate = new CompositeDirectConverter[String,Date](dateFormats.map(new ParseFormatConverter[Date](_)))
  lazy val dateToString = formatToString[Date](DateFormat.getDateInstance)

  def stringToEnum[T <: Enumeration#Value](enumeration: Enumeration): Converter[String,T] = new Converter[String,T] {
    def convert(from: String) = enumeration.values.find(_.toString == from).map(_.asInstanceOf[T])
  }
}