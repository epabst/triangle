package com.github.triangle

import converter.ValueFormat

/** A wrapping PortableField that converts between types. */
class ConvertedField[T,F](val delegate: PortableField[F], val converter: F => Option[T], val unconverter: T => Option[F])
    extends SimplePortableField[T](delegate.getterVal.andThen(value => value.flatMap(converter(_))),
      _updater = {
        case input @ UpdaterInput(subject, valueOpt, context) if delegate.updaterVal.isDefinedAt(input.withUndeterminedValue) =>
          val unconvertedValue: Option[F] = valueOpt.flatMap(unconverter(_))
          delegate.updaterVal(input.withValue(unconvertedValue))
      }) with FieldWithDelegate[T] {
  override lazy val toString = "converted(" + delegate + ")"
}

case class FormattedField[T](format: ValueFormat[T], field: PortableField[String])
    extends ConvertedField[T,String](field, format.toValue, v => Some(format.toString(v))) {
  override lazy val toString = "formatted(" + format + ", " + field + ")"
}
