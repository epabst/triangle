package com.github.triangle

/** A wrapping PortableField that converts between types. */
abstract class ConvertedField[T,F](field: PortableField[F]) extends FieldWithDelegate[T] {
  protected def delegate = field

  def convert(value: F): Option[T]

  def unconvert(value: T): Option[F]

  def getter = field.getter.andThen(value => value.flatMap(convert(_)))

  def setter = field.setter.andThen(setter => setter.compose(value => value.flatMap(unconvert(_))))

  def transformer[S <: AnyRef]: PartialFunction[S,Option[T] => S] = {
    case subject: S if field.transformer[S].isDefinedAt(subject) => { value =>
      val unconvertedValue: Option[F] = value.flatMap(unconvert(_))
      field.transformer[S].apply(subject)(unconvertedValue)
    }
  }

  override def toString = "converted(" + field + ")"
}

case class FormattedField[T](format: ValueFormat[T], field: PortableField[String]) extends ConvertedField[T,String](field) {
  def convert(string: String) = format.toValue(string)

  def unconvert(value: T) = Some(format.toString(value))

  override def toString = "formatted(" + format + ", " + field + ")"
}
