package com.github.triangle

/**
 * A Seq of fields of the same value type.  It results from PortableField.+(...).
 * @author Eric Pabst (epabst@gmail.com)
 *         Date: 8/25/12
 *         Time: 1:02 PM
 */
case class TypedFieldSeq[T](fields: Vector[PortableField[T]])
    extends SimplePortableField[T]({
      val getters = fields.map(field => field.getterVal)
      new PartialFunct[GetterInput,Option[T]] {
        def isDefinedAt(input: GetterInput) = getters.exists(_.isDefinedAt(input))

        def attempt(input: GetterInput) = {
          val lazyValueOpts = getters.view.flatMap(_.attemptAndCheckForNulls(input))
          lazyValueOpts.find(_.isDefined).orElse(lazyValueOpts.headOption)
        }
      }
    }, {
      /** Combines the updaters, calling all of them (not stopping when one works). */
      new PartialFunct[UpdaterInput[AnyRef, T], AnyRef] {
        val updaters = fields.map(_.updaterVal[AnyRef])

        def isDefinedAt(input: UpdaterInput[AnyRef, T]) = updaters.exists(_.isDefinedAt(input))

        def attempt(input: UpdaterInput[AnyRef, T]) =
          updaters.foldLeft(PartialResult(false, input.subject)) { (partialResult, updater) =>
            val updatedInput = input.copy(subject = partialResult.tentativeResult)
            updater.attempt(updatedInput) match {
              case Some(updatedSubject) =>
                PartialResult(defined = true, updatedSubject)
              case None =>
                partialResult
            }
          }.toOption
      }
    }) {

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

  override lazy val toString = fields.mkString(" + ")
}

private case class PartialResult[T](defined: Boolean, tentativeResult: T) {
  def toOption: Option[T] = if (defined) Some(tentativeResult) else None
}

object TypedFieldSeq {
  def apply[T](fields: PortableField[T]*): TypedFieldSeq[T] = TypedFieldSeq(Vector(fields:_*))
}
