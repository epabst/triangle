package com.github.triangle.converter

import java.text.{DateFormat, Format, NumberFormat}
import java.util.{Calendar, Date}

/**
 * A converter from one type to another.
 * @author Eric Pabst (epabst@gmail.com)
 *         Date: 2/22/13
 *         Time: 7:44 AM
 */
trait Converter[-A,+B] {
  /** Converts from {{{from}}} to the new type if possible. */
  def convert(from: A): Option[B]
}

object Converter {
  /**
   * This is a polymorphic singleton where a single instance is used to implement any number of types.
   * This is so that it can be used to identify when a Converter isn't doing anything and may be bypassed.
   */
  def identityConverter[A]: Converter[A,A] = _identityConverter.asInstanceOf[Converter[A,A]]

  private val _identityConverter = new Converter[Any,Any] {
    def convert(from: Any) = Some(from)
  }

  implicit def apply[A,B](f: A => Option[B]): Converter[A,B] = new Converter[A,B] {
    def convert(from: A) = f(from)
  }

  val anyToString: Converter[Any,String] = new Converter[Any,String] {
    def convert(from: Any) = Some(from.toString)
  }

  val noConverter: Converter[Any,Nothing] = new Converter[Any,Nothing] {
    def convert(from: Any) = None
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

  private lazy val percentagePercentFormat = NumberFormat.getPercentInstance
  private lazy val percentageEditNumberFormat = {
    val editFormat = NumberFormat.getNumberInstance
    editFormat.setMinimumFractionDigits(percentagePercentFormat.getMinimumFractionDigits)
    editFormat.setMaximumFractionDigits(percentagePercentFormat.getMaximumFractionDigits)
    editFormat
  }
  val stringToPercentage: Converter[String,Float] = new CompositeDirectConverter[String,Float](
    new ParseFormatConverter[Float](percentagePercentFormat, _.asInstanceOf[Number].floatValue()) +:
      List(percentageEditNumberFormat, NumberFormat.getNumberInstance).map(
        new ParseFormatConverter[Float](_, _.asInstanceOf[Number].floatValue() / 100))
  )
  lazy val percentageToEditString: Converter[Float,String] = new Converter[Float,String] {
    def convert(from: Float) = formatToString[Float](percentageEditNumberFormat).convert(from * 100)
  }
  lazy val percentageToString: Converter[Float,String] = new Converter[Float,String] {
    def convert(from: Float) = formatToString[Float](percentagePercentFormat).convert(from)
  }

  lazy val dateToLong = Converter[Date, Long](d => Some(d.getTime))
  lazy val longToDate = Converter[Long, Date](l => Some(new Date(l)))

  lazy val dateToCalendar = Converter[Date, Calendar] { date =>
    val calendar = Calendar.getInstance
    calendar.setTime(date)
    Some(calendar)
  }
  lazy val calendarToDate = Converter[Calendar, Date](c => Some(c.getTime))

  def formatToString[T](format: Format): Converter[T,String] = Converter[T,String](value => Some(format.format(value)))

  lazy val currencyToString: Converter[Double,String] = formatToString[Double](currencyFormat)
  lazy val currencyToEditString: Converter[Double,String] = formatToString[Double](currencyEditFormat)

  private lazy val dateFormats = List(DateFormat.SHORT, DateFormat.DEFAULT, DateFormat.MEDIUM).map(DateFormat.getDateInstance(_)) ++
          List("MM/dd/yyyy", "yyyy-MM-dd", "dd MMM yyyy").map(new java.text.SimpleDateFormat(_))
  lazy val stringToDate = new CompositeDirectConverter[String,Date](dateFormats.map(new ParseFormatConverter[Date](_)))
  //SHORT is probably the best style for input
  lazy val dateToString = formatToString[Date](DateFormat.getDateInstance(DateFormat.SHORT))
  //DEFAULT is probably the best style for output
  lazy val dateToDisplayString = formatToString[Date](DateFormat.getDateInstance(DateFormat.DEFAULT))

  def stringToEnum[T <: Enumeration#Value](enumeration: Enumeration): Converter[String,T] = new Converter[String,T] {
    def convert(from: String) = enumeration.values.find(_.toString == from).map(_.asInstanceOf[T])
  }
}
