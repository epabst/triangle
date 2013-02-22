package com.github.triangle

import converter.ValueFormat

/** A wrapping PortableField that converts between types. */
abstract class ConvertedField[T,F](field: PortableField[F]) extends FieldWithDelegate[T] {
  protected val delegate = field

  def convert(value: F): Option[T]

  def unconvert(value: T): Option[F]

  def getter = field.getterVal.andThen(value => value.flatMap(convert(_)))

  def updater[S <: AnyRef]: PartialFunction[UpdaterInput[S,T],S] = {
    case input @ UpdaterInput(subject, valueOpt, context) if field.updaterVal.isDefinedAt(input.withUndeterminedValue) =>
      val unconvertedValue: Option[F] = valueOpt.flatMap(unconvert(_))
      field.updaterVal(input.withValue(unconvertedValue))
  }

  override lazy val toString = "converted(" + field + ")"
}

case class FormattedField[T](format: ValueFormat[T], field: PortableField[String]) extends ConvertedField[T,String](field) {
  def convert(string: String) = format.toValue(string)

  def unconvert(value: T) = Some(format.toString(value))

  override lazy val toString = "formatted(" + format + ", " + field + ")"
}
