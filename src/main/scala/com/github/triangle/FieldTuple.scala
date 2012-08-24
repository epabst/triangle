package com.github.triangle

import scala.AnyRef

/** A Tuple of Fields.
  * Use the right subtype for the correct -arity such as FieldTuple3.
  * @author Eric Pabst (epabst@gmail.com)
  */
trait FieldTuple extends TypedProduct[PortableField[_]] {
  /** a type which is a Tuple for the field values such as (Option[A], Option[B], Option[C]). */
  type ValuesTuple <: Product

  /** Gets an empty Tuple. */
  def emptyValuesTuple: ValuesTuple

  /** Gets a Tuple with the results of calling each getterFromItem with {{{items}}} as the parameter. */
  def valuesTuple(items: GetterInput): ValuesTuple

  /** Updates {{{subject}}} with the values. */
  def updateWithValues[S <: AnyRef,T](subject: S, values: ValuesTuple): S

  //this is only here to help the IDE to infer the type concretely
  override def productIterator: Iterator[PortableField[_]] = super.productIterator.asInstanceOf[Iterator[PortableField[_]]]

  trait TupleField[T] extends PortableField[T] { selfField =>
    /** Allows chaining such as {{{FieldTuple(...).asGetter(...).withUpdater(...)}}}. */
    def withUpdater(splitter: T => ValuesTuple): PortableField[T] = {
      val updaterField = FieldTuple.this.asUpdater(splitter)
      new Field[T](selfField + updaterField) {
        override def deepCollect[R](f: PartialFunction[BaseField, R]) = {
          val lifted = f.lift
          //don't traverse theTransformer since it duplicates the fields within selfField
          lifted(this).orElse(lifted(selfField)).map(Seq(_)).getOrElse(selfField.deepCollect(f))
        }
      }
    }

    override def deepCollect[R](f: PartialFunction[BaseField, R]): Seq[R] = {
      val lifted = f.lift
      productIterator.toSeq.flatMap(field => lifted(field).map(Seq(_)).getOrElse(field.deepCollect(f)))
    }
  }

  /** Converts the FieldTuple to a Getter PortableField that accepts a composite type T and a combiner function. */
  def asGetter[T](combiner: ValuesTuple => Option[T]): TupleField[T] = {
    new Getter[T] with TupleField[T] {
      def getter = {
        case input if productIterator.forall(_.getter.isDefinedAt(input)) => combiner(valuesTuple(input))
      }
    }
  }

  /** Creates a PortableField with an Updater that accepts a composite type T and a splitter function. */
  def asUpdater[T](splitter: T => ValuesTuple): TupleField[T] = {
    new Updater[T] with TupleField[T] {
      def updater[S <: AnyRef]: PartialFunction[UpdaterInput[S,T],S] = {
        case input @ UpdaterInput(subject, valueOpt, _) if productIterator.forall(_.updater.isDefinedAt(input.withUndeterminedValue)) =>
          updateWithValues(subject, valueOpt.map(splitter(_)).getOrElse(emptyValuesTuple))
      }
    }
  }

  def canEqual(that: Any) = that match {
    case x: AnyRef => this.getClass == x.getClass
    case _ => false
  }
}

/** The implicit toTupleXOfSomes defs are useful when defining a updater for a FieldTuple. */
object FieldTuple {
  def apply[F1,F2](_1: PortableField[F1], _2: PortableField[F2]) =
    FieldTuple2(_1, _2)
  def apply[F1,F2,F3](_1: PortableField[F1], _2: PortableField[F2], _3: PortableField[F3]) =
    FieldTuple3(_1, _2, _3)
  def apply[F1,F2,F3,F4](_1: PortableField[F1], _2: PortableField[F2], _3: PortableField[F3], _4: PortableField[F4]) =
    FieldTuple4(_1, _2, _3, _4)
  def apply[F1,F2,F3,F4,F5](_1: PortableField[F1], _2: PortableField[F2], _3: PortableField[F3], _4: PortableField[F4],
                            _5: PortableField[F5]) =
    FieldTuple5(_1, _2, _3, _4, _5)
  def apply[F1,F2,F3,F4,F5,F6](_1: PortableField[F1], _2: PortableField[F2], _3: PortableField[F3],
                               _4: PortableField[F4], _5: PortableField[F5], _6: PortableField[F6]) =
    FieldTuple6(_1, _2, _3, _4, _5, _6)
  def apply[F1,F2,F3,F4,F5,F6,F7](_1: PortableField[F1], _2: PortableField[F2], _3: PortableField[F3],
                                  _4: PortableField[F4], _5: PortableField[F5], _6: PortableField[F6],
                                  _7: PortableField[F7]) =
    FieldTuple7(_1, _2, _3, _4, _5, _6, _7)

  implicit def toTuple2OfSomes[F1,F2](tuple: (F1, F2)): (Option[F1], Option[F2]) =
    (Some(tuple._1), Some(tuple._2))

  implicit def toTuple3OfSomes[F1,F2,F3](tuple: (F1, F2, F3)): (Option[F1], Option[F2], Option[F3]) =
    (Some(tuple._1), Some(tuple._2), Some(tuple._3))

  implicit def toTuple4OfSomes[F1,F2,F3,F4](tuple: (F1, F2, F3, F4)): (Option[F1], Option[F2], Option[F3], Option[F4]) =
    (Some(tuple._1), Some(tuple._2), Some(tuple._3), Some(tuple._4))
}

/** A Product where the items share a base type. */
trait TypedProduct[T <: Any] extends Product {
  override def productIterator: Iterator[T] = super.productIterator.map(_.asInstanceOf[T])
}

case class FieldTuple2[F1,F2](_1: PortableField[F1], _2: PortableField[F2])
        extends FieldTuple with Product2[PortableField[F1],PortableField[F2]] {
  type ValuesTuple = (Option[F1], Option[F2])

  def emptyValuesTuple = (None, None)

  def valuesTuple(items: GetterInput) = (_1.getter(items), _2.getter(items))

  def updateWithValues[S <: AnyRef,T](subject: S, values: ValuesTuple): S = {
    val result1 = _1.updateWithValue(subject, values._1)
    _2.updateWithValue(result1, values._2)
  }
}

case class FieldTuple3[F1,F2,F3](_1: PortableField[F1], _2: PortableField[F2], _3: PortableField[F3])
        extends FieldTuple with Product3[PortableField[F1],PortableField[F2],PortableField[F3]] {
  type ValuesTuple = (Option[F1], Option[F2], Option[F3])

  def emptyValuesTuple = (None, None, None)

  def valuesTuple(items: GetterInput) = (_1.getter(items), _2.getter(items), _3.getter(items))

  def updateWithValues[S <: AnyRef,T](subject: S, values: ValuesTuple): S = {
    val result1 = _1.updateWithValue(subject, values._1)
    val result2 = _2.updateWithValue(result1, values._2)
    _3.updateWithValue(result2, values._3)
  }
}

case class FieldTuple4[F1,F2,F3,F4](_1: PortableField[F1], _2: PortableField[F2], _3: PortableField[F3],
                                    _4: PortableField[F4])
        extends FieldTuple with Product4[PortableField[F1],PortableField[F2],PortableField[F3],PortableField[F4]] {
  type ValuesTuple = (Option[F1], Option[F2], Option[F3], Option[F4])

  def emptyValuesTuple = (None, None, None, None)

  def valuesTuple(items: GetterInput) = (_1.getter(items), _2.getter(items), _3.getter(items),
          _4.getter(items))

  def updateWithValues[S <: AnyRef,T](subject: S, values: ValuesTuple): S = {
    val result1 = _1.updateWithValue(subject, values._1)
    val result2 = _2.updateWithValue(result1, values._2)
    val result3 = _3.updateWithValue(result2, values._3)
    _4.updateWithValue(result3, values._4)
  }
}

case class FieldTuple5[F1,F2,F3,F4,F5](_1: PortableField[F1], _2: PortableField[F2], _3: PortableField[F3],
                                       _4: PortableField[F4], _5: PortableField[F5])
        extends FieldTuple with Product5[PortableField[F1],PortableField[F2],PortableField[F3],PortableField[F4],PortableField[F5]] {
  type ValuesTuple = (Option[F1], Option[F2], Option[F3], Option[F4], Option[F5])

  def emptyValuesTuple = (None, None, None, None, None)

  def valuesTuple(items: GetterInput) = (_1.getter(items), _2.getter(items), _3.getter(items),
          _4.getter(items), _5.getter(items))

  def updateWithValues[S <: AnyRef,T](subject: S, values: ValuesTuple): S = {
    val result1 = _1.updateWithValue(subject, values._1)
    val result2 = _2.updateWithValue(result1, values._2)
    val result3 = _3.updateWithValue(result2, values._3)
    val result4 = _4.updateWithValue(result3, values._4)
    _5.updateWithValue(result4, values._5)
  }
}

case class FieldTuple6[F1,F2,F3,F4,F5,F6](_1: PortableField[F1], _2: PortableField[F2], _3: PortableField[F3],
                                          _4: PortableField[F4], _5: PortableField[F5], _6: PortableField[F6])
        extends FieldTuple with Product6[PortableField[F1],PortableField[F2],PortableField[F3],PortableField[F4],PortableField[F5],PortableField[F6]] {
  type ValuesTuple = (Option[F1], Option[F2], Option[F3], Option[F4], Option[F5], Option[F6])

  def emptyValuesTuple = (None, None, None, None, None, None)

  def valuesTuple(items: GetterInput) = (_1.getter(items), _2.getter(items), _3.getter(items),
          _4.getter(items), _5.getter(items), _6.getter(items))

  def updateWithValues[S <: AnyRef,T](subject: S, values: ValuesTuple): S = {
    val result1 = _1.updateWithValue(subject, values._1)
    val result2 = _2.updateWithValue(result1, values._2)
    val result3 = _3.updateWithValue(result2, values._3)
    val result4 = _4.updateWithValue(result3, values._4)
    val result5 = _5.updateWithValue(result4, values._5)
    _6.updateWithValue(result5, values._6)
  }
}

case class FieldTuple7[F1,F2,F3,F4,F5,F6,F7](_1: PortableField[F1], _2: PortableField[F2], _3: PortableField[F3],
                                             _4: PortableField[F4], _5: PortableField[F5], _6: PortableField[F6],
                                             _7: PortableField[F7])
        extends FieldTuple with Product7[PortableField[F1],PortableField[F2],PortableField[F3],PortableField[F4],PortableField[F5],PortableField[F6],PortableField[F7]] {
  type ValuesTuple = (Option[F1], Option[F2], Option[F3], Option[F4], Option[F5], Option[F6], Option[F7])

  def emptyValuesTuple = (None, None, None, None, None, None, None)

  def valuesTuple(items: GetterInput) = (_1.getter(items), _2.getter(items), _3.getter(items),
          _4.getter(items), _5.getter(items), _6.getter(items), _7.getter(items))

  def updateWithValues[S <: AnyRef,T](subject: S, values: ValuesTuple): S = {
    val result1 = _1.updateWithValue(subject, values._1)
    val result2 = _2.updateWithValue(result1, values._2)
    val result3 = _3.updateWithValue(result2, values._3)
    val result4 = _4.updateWithValue(result3, values._4)
    val result5 = _5.updateWithValue(result4, values._5)
    val result6 = _6.updateWithValue(result5, values._6)
    _7.updateWithValue(result6, values._7)
  }
}
