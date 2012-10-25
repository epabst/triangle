package com.github.triangle

/**
 * A Seq of fields of the same value type.  It results from PortableField.+(...).
 * @author Eric Pabst (epabst@gmail.com)
 *         Date: 8/25/12
 *         Time: 1:02 PM
 */
case class TypedFieldSeq[T](fields: Vector[PortableField[T]]) extends PortableField[T] {
  private lazy val getters: Seq[PartialFunction[GetterInput, Option[T]]] = fields.map(field => field.getterVal)

  def getter = {
    case items if getters.exists(_.isDefinedAt(items)) =>
      val definedGetters = getters.filter(_.isDefinedAt(items))
      val values = definedGetters.map(_.apply(items))
      values.find(_.isDefined).getOrElse(None)
  }

  /** Combines the two updaters, calling only applicable ones (not just the first though). */
  def updater[S <: AnyRef] = {
    val updaters = fields.map(_.updaterVal[S])
    val _updater: PartialFunction[UpdaterInput[S,T],S] = {
      case input @ UpdaterInput(subject, valueOpt, context) if updaters.exists(_.isDefinedAt(input)) =>
        val definedUpdaters = updaters.filter(_.isDefinedAt(input))
        definedUpdaters.foldLeft(subject)((subject, updater) => updater.apply(input.copy(subject = subject)))
    }
    _updater
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
