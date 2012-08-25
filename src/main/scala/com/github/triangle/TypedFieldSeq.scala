package com.github.triangle

/**
 * A Seq of fields of the same value type.  It results from PortableField.+(...).
 * @author Eric Pabst (epabst@gmail.com)
 *         Date: 8/25/12
 *         Time: 1:02 PM
 */
case class TypedFieldSeq[T](fields: Vector[PortableField[T]]) extends PortableField[T] {
  def getter = {
    case items if fields.exists(_.getter.isDefinedAt(items)) =>
      val values = fields.view.map(_.getter).filter(_.isDefinedAt(items)).map(_(items))
      values.find(_.isDefined).getOrElse(None)
  }

  /** Combines the two updaters, calling only applicable ones (not just the first though). */
  def updater[S <: AnyRef] = {
    case input @ UpdaterInput(subject, valueOpt, context) if fields.exists(_.updater.isDefinedAt(input)) =>
      val definedFields = fields.filter(_.updater.isDefinedAt(input))
      definedFields.foldLeft(subject)((subject, field) => field.updater(input.copy(subject = subject)))
  }

  override def deepCollect[R](f: PartialFunction[BaseField, R]): Seq[R] = {
    super.deepCollect[R](f) match {
      case Nil =>
        val lifted = f.lift
        fields.flatMap(field => lifted(field).map(Seq[R](_)).getOrElse(field.deepCollect(f)))
      case x => x
    }
  }

  /**Adds two PortableField objects together. */
  override def +(other: PortableField[T]) = TypedFieldSeq(fields :+ other)

  override def toString = fields.mkString(" + ")
}

object TypedFieldSeq {
  def apply[T](fields: PortableField[T]*): TypedFieldSeq[T] = TypedFieldSeq(Vector(fields:_*))
}
