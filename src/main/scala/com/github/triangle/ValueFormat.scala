package com.github.triangle

import java.text.{ParsePosition, Format}
import java.util.{Calendar, Date}
import scala.Enumeration
import Converter._

/**
 * A value format.
 * @author Eric Pabst (epabst@gmail.com)
 * Date: 2/4/11
 * Time: 9:25 PM
 */
trait ValueFormat[T] {
  /** May need to be overridden */
  def toString(value: T): String = value.toString

  def toValue(s: String): Option[T]
}

class ConvertingValueFormat[T](toValueConverter: Converter[String,T],
                               toStringConverter: Converter[T,String] = anyToString) extends ValueFormat[T] {
  override def toString(value: T) = toStringConverter.convert(value).getOrElse(throw new IllegalArgumentException(String.valueOf(value)))

  def toValue(s: String) = toValueConverter.convert(s)
}

class GenericConvertingValueFormat[T](toValueConverter: GenericConverter[String,T],
                                      toStringConverter: Converter[T, String] = anyToString)(implicit manifest: Manifest[T])
        extends ValueFormat[T] {
  override def toString(value: T) = toStringConverter.convert(value).getOrElse(throw new IllegalArgumentException(String.valueOf(value)))

  def toValue(s: String) = toValueConverter.convertTo[T](s)
}

class TextValueFormat[T](format: Format, obj2Value: (Object) => T = {(v: Object) => v.asInstanceOf[T]}) extends ValueFormat[T] {
  override def toString(value: T) = format.format(value)

  def toValue(string: String) = {
    val position = new ParsePosition(0)
    val result = format.parseObject(string, position)
    if (position.getIndex == 0) None else Some(obj2Value(result))
  }
}

class FlexibleValueFormat[T](formats: List[ValueFormat[T]]) extends ValueFormat[T] {
  override def toString(value: T) = formats.headOption.map(_.toString(value)).getOrElse(super.toString(value))

  def toValue(s: String): Option[T] = formats.view.flatMap(_.toValue(s)).headOption
}

object ValueFormat {
  lazy val dateFormats = List(new java.text.SimpleDateFormat("MM/dd/yyyy"), new java.text.SimpleDateFormat("dd MMM yyyy")).map(new TextValueFormat[Date](_))

  def convertingFormat[T](toValueConverter: Converter[String,T], toStringConverter: Converter[T,String] = anyToString) =
    new ConvertingValueFormat[T](toValueConverter, toStringConverter)

  def toCalendarFormat(format: ValueFormat[Date]): ValueFormat[Calendar] = new ValueFormat[Calendar] {
    override def toString(value: Calendar) = format.toString(value.getTime)

    def toValue(s: String) = format.toValue(s).map { date =>
      val calendar = Calendar.getInstance
      calendar.setTime(date)
      calendar
    }
  }

  def enumFormat[T <: Enumeration#Value](enum: Enumeration): ValueFormat[T] = new ValueFormat[T] {
    override def toString(enumValue: T) = enumValue.toString

    def toValue(s: String) = enum.values.find(_.toString == s).map(_.asInstanceOf[T])
  }

  def basicFormat[T <: AnyVal](implicit manifest: Manifest[T]): ValueFormat[T] =
    new GenericConvertingValueFormat[T](stringToAnyVal, anyToString)

  lazy val currencyValueFormat = convertingFormat(stringToCurrency, currencyToEditString)
  lazy val currencyDisplayValueFormat = convertingFormat(stringToCurrency, currencyToString)
  lazy val dateValueFormat = new FlexibleValueFormat[Date](dateFormats)
  lazy val calendarValueFormat = toCalendarFormat(dateValueFormat)
}
