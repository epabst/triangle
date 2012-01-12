package com.github.triangle

import java.text.{DateFormat, Format, ParsePosition, NumberFormat}
import java.util.{Calendar, Date}

trait Converter[-A,+B] {
  /** Converts from {{{from}}} to the new type if possible. */
  def convert(from: A): Option[B]
}

abstract class SimpleConverter[-A,+B] extends Converter[A,B] {
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

class CompositeDirectConverter[-A,+B](converters: List[Converter[A,B]]) extends Converter[A,B] {
  def convert(from: A) = converters.view.flatMap(_.convert(from)).headOption
}

object Converter {
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