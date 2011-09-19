package com.github.triangle

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

object ValueFormat {
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

  def enumFormat[T <: Enumeration#Value](enum: Enumeration): ValueFormat[T] = convertingFormat[T](stringToEnum(enum))

  def basicFormat[T <: AnyVal](implicit manifest: Manifest[T]): ValueFormat[T] =
    new GenericConvertingValueFormat[T](stringToAnyVal, anyToString)

  lazy val currencyValueFormat = convertingFormat(stringToCurrency, currencyToEditString)
  lazy val currencyDisplayValueFormat = convertingFormat(stringToCurrency, currencyToString)
  lazy val dateValueFormat = convertingFormat(stringToDate, dateToString)
  lazy val calendarValueFormat = toCalendarFormat(dateValueFormat)
}
