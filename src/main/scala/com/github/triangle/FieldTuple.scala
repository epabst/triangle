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

  /** Gets a Tuple with the results of calling each getter with {{{readable}}} as the parameter. */
  def valuesTuple(readable: AnyRef): ValuesTuple

  /** Gets a Tuple with the results of calling each getterFromItem with {{{items}}} as the parameter. */
  def valuesTupleFromItem(items: List[AnyRef]): ValuesTuple

  /** Sets the values into {{{subject}}}. */
  def setValues[T](subject: AnyRef, values: ValuesTuple)

  /** Transforms {{{subject}}} with the values. */
  def transformWithValues[S <: AnyRef,T](subject: S, values: ValuesTuple): S

  //this is only here to help the IDE to infer the type concretely
  override def productIterator: Iterator[PortableField[_]] = super.productIterator

  trait TupleField[T] extends PortableField[T] { selfField =>
    /** Allows chaining such as {{{FieldTuple(...).getter(...).withTransformer(...)}}}. */
    def withTransformer(splitter: T => ValuesTuple): PortableField[T] = {
      val theTransformer = FieldTuple.this.transformer(splitter)
      new Field[T](selfField + theTransformer) {
        override def deepCollect[R](f: PartialFunction[BaseField, R]) = {
          val lifted = f.lift
          //don't traverse theTransformer since it duplicates the fields within selfField
          lifted(this).orElse(lifted(selfField)).map(List(_)).getOrElse(selfField.deepCollect(f))
        }
      }
    }

    override def deepCollect[R](f: PartialFunction[BaseField, R]): List[R] = {
      val lifted = f.lift
      productIterator.toList.flatMap(field => lifted(field).map(List(_)).getOrElse(field.deepCollect(f)))
    }
  }

  /** Creates a Getter PortableField that accepts a composite type T and a combiner function. */
  def getter[T](combiner: ValuesTuple => Option[T]): TupleField[T] = {
    new Getter[T] with TupleField[T] {
      def getter = {
        case ref if productIterator.forall(_.getter.isDefinedAt(ref)) => combiner(valuesTuple(ref))
      }
    }
  }

  /** Creates a Transformer and Setter PortableField that accepts a composite type T and a splitter function. */
  def transformer[T](splitter: T => ValuesTuple): NoGetter[T] = {
    new NoGetter[T] with TupleField[T] {
      def transformer[S <: AnyRef]: PartialFunction[S,Option[T] => S] = {
        case ref if productIterator.forall(_.transformer.isDefinedAt(ref)) => (valueOpt: Option[T]) =>
          transformWithValues(ref, valueOpt.map(splitter(_)).getOrElse(emptyValuesTuple))
      }

      def setter = {
        case ref if productIterator.forall(_.setter.isDefinedAt(ref)) => (valueOpt: Option[T]) =>
          setValues(ref, valueOpt.map(splitter(_)).getOrElse(emptyValuesTuple))
      }
    }
  }

  def canEqual(that: Any) = that match {
    case x: AnyRef => this.getClass == x.getClass
    case _ => false
  }
}

/** The implicit toTupleXOfSomes defs are useful when defining a transformer for a FieldTuple. */
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
  def valuesTuple(readable: AnyRef) = (_1.getter(readable), _2.getter(readable))
  def valuesTupleFromItem(items: List[AnyRef]) = (_1.getterFromItem(items), _2.getterFromItem(items))
  def setValues[T](subject: AnyRef, values: ValuesTuple) {
    _1.setter(subject)(values._1)
    _2.setter(subject)(values._2)
  }
  def transformWithValues[S <: AnyRef,T](subject: S, values: ValuesTuple): S = {
    val result1 = _1.transformWithValue(subject, values._1)
    _2.transformWithValue(result1, values._2)
  }
}

case class FieldTuple3[F1,F2,F3](_1: PortableField[F1], _2: PortableField[F2], _3: PortableField[F3])
        extends FieldTuple with Product3[PortableField[F1],PortableField[F2],PortableField[F3]] {
  type ValuesTuple = (Option[F1], Option[F2], Option[F3])
  def emptyValuesTuple = (None, None, None)
  def valuesTuple(readable: AnyRef) = (_1.getter(readable), _2.getter(readable), _3.getter(readable))
  def valuesTupleFromItem(items: List[AnyRef]) = (_1.getterFromItem(items), _2.getterFromItem(items), _3.getterFromItem(items))
  def setValues[T](subject: AnyRef, values: ValuesTuple) {
    _1.setter(subject)(values._1)
    _2.setter(subject)(values._2)
    _3.setter(subject)(values._3)
  }
  def transformWithValues[S <: AnyRef,T](subject: S, values: ValuesTuple): S = {
    val result1 = _1.transformWithValue(subject, values._1)
    val result2 = _2.transformWithValue(result1, values._2)
    _3.transformWithValue(result2, values._3)
  }
}

case class FieldTuple4[F1,F2,F3,F4](_1: PortableField[F1], _2: PortableField[F2], _3: PortableField[F3],
                                    _4: PortableField[F4])
        extends FieldTuple with Product4[PortableField[F1],PortableField[F2],PortableField[F3],PortableField[F4]] {
  type ValuesTuple = (Option[F1], Option[F2], Option[F3], Option[F4])
  def emptyValuesTuple = (None, None, None, None)
  def valuesTuple(readable: AnyRef) = (_1.getter(readable), _2.getter(readable), _3.getter(readable), _4.getter(readable))
  def valuesTupleFromItem(items: List[AnyRef]) = (_1.getterFromItem(items), _2.getterFromItem(items), _3.getterFromItem(items),
          _4.getterFromItem(items))
  def setValues[T](subject: AnyRef, values: ValuesTuple) {
    _1.setter(subject)(values._1)
    _2.setter(subject)(values._2)
    _3.setter(subject)(values._3)
    _4.setter(subject)(values._4)
  }
  def transformWithValues[S <: AnyRef,T](subject: S, values: ValuesTuple): S = {
    val result1 = _1.transformWithValue(subject, values._1)
    val result2 = _2.transformWithValue(result1, values._2)
    val result3 = _3.transformWithValue(result2, values._3)
    _4.transformWithValue(result3, values._4)
  }
}

case class FieldTuple5[F1,F2,F3,F4,F5](_1: PortableField[F1], _2: PortableField[F2], _3: PortableField[F3],
                                       _4: PortableField[F4], _5: PortableField[F5])
        extends FieldTuple with Product5[PortableField[F1],PortableField[F2],PortableField[F3],PortableField[F4],PortableField[F5]] {
  type ValuesTuple = (Option[F1], Option[F2], Option[F3], Option[F4], Option[F5])
  def emptyValuesTuple = (None, None, None, None, None)
  def valuesTuple(readable: AnyRef) = (_1.getter(readable), _2.getter(readable), _3.getter(readable),
          _4.getter(readable), _5.getter(readable))
  def valuesTupleFromItem(items: List[AnyRef]) = (_1.getterFromItem(items), _2.getterFromItem(items), _3.getterFromItem(items),
          _4.getterFromItem(items), _5.getterFromItem(items))
  def setValues[T](subject: AnyRef, values: ValuesTuple) {
    _1.setter(subject)(values._1)
    _2.setter(subject)(values._2)
    _3.setter(subject)(values._3)
    _4.setter(subject)(values._4)
    _5.setter(subject)(values._5)
  }
  def transformWithValues[S <: AnyRef,T](subject: S, values: ValuesTuple): S = {
    val result1 = _1.transformWithValue(subject, values._1)
    val result2 = _2.transformWithValue(result1, values._2)
    val result3 = _3.transformWithValue(result2, values._3)
    val result4 = _4.transformWithValue(result3, values._4)
    _5.transformWithValue(result4, values._5)
  }
}

case class FieldTuple6[F1,F2,F3,F4,F5,F6](_1: PortableField[F1], _2: PortableField[F2], _3: PortableField[F3],
                                          _4: PortableField[F4], _5: PortableField[F5], _6: PortableField[F6])
        extends FieldTuple with Product6[PortableField[F1],PortableField[F2],PortableField[F3],PortableField[F4],PortableField[F5],PortableField[F6]] {
  type ValuesTuple = (Option[F1], Option[F2], Option[F3], Option[F4], Option[F5], Option[F6])
  def emptyValuesTuple = (None, None, None, None, None, None)
  def valuesTuple(readable: AnyRef) = (_1.getter(readable), _2.getter(readable), _3.getter(readable),
          _4.getter(readable), _5.getter(readable), _6.getter(readable))
  def valuesTupleFromItem(items: List[AnyRef]) = (_1.getterFromItem(items), _2.getterFromItem(items), _3.getterFromItem(items),
          _4.getterFromItem(items), _5.getterFromItem(items), _6.getterFromItem(items))
  def setValues[T](subject: AnyRef, values: ValuesTuple) {
    _1.setter(subject)(values._1)
    _2.setter(subject)(values._2)
    _3.setter(subject)(values._3)
    _4.setter(subject)(values._4)
    _5.setter(subject)(values._5)
    _6.setter(subject)(values._6)
  }
  def transformWithValues[S <: AnyRef,T](subject: S, values: ValuesTuple): S = {
    val result1 = _1.transformWithValue(subject, values._1)
    val result2 = _2.transformWithValue(result1, values._2)
    val result3 = _3.transformWithValue(result2, values._3)
    val result4 = _4.transformWithValue(result3, values._4)
    val result5 = _5.transformWithValue(result4, values._5)
    _6.transformWithValue(result5, values._6)
  }
}

case class FieldTuple7[F1,F2,F3,F4,F5,F6,F7](_1: PortableField[F1], _2: PortableField[F2], _3: PortableField[F3],
                                             _4: PortableField[F4], _5: PortableField[F5], _6: PortableField[F6],
                                             _7: PortableField[F7])
        extends FieldTuple with Product7[PortableField[F1],PortableField[F2],PortableField[F3],PortableField[F4],PortableField[F5],PortableField[F6],PortableField[F7]] {
  type ValuesTuple = (Option[F1], Option[F2], Option[F3], Option[F4], Option[F5], Option[F6], Option[F7])
  def emptyValuesTuple = (None, None, None, None, None, None, None)
  def valuesTuple(readable: AnyRef) = (_1.getter(readable), _2.getter(readable), _3.getter(readable),
          _4.getter(readable), _5.getter(readable), _6.getter(readable), _7.getter(readable))
  def valuesTupleFromItem(items: List[AnyRef]) = (_1.getterFromItem(items), _2.getterFromItem(items), _3.getterFromItem(items),
          _4.getterFromItem(items), _5.getterFromItem(items), _6.getterFromItem(items), _7.getterFromItem(items))
  def setValues[T](subject: AnyRef, values: ValuesTuple) {
    _1.setter(subject)(values._1)
    _2.setter(subject)(values._2)
    _3.setter(subject)(values._3)
    _4.setter(subject)(values._4)
    _5.setter(subject)(values._5)
    _6.setter(subject)(values._6)
    _7.setter(subject)(values._7)
  }
  def transformWithValues[S <: AnyRef,T](subject: S, values: ValuesTuple): S = {
    val result1 = _1.transformWithValue(subject, values._1)
    val result2 = _2.transformWithValue(result1, values._2)
    val result3 = _3.transformWithValue(result2, values._3)
    val result4 = _4.transformWithValue(result3, values._4)
    val result5 = _5.transformWithValue(result4, values._5)
    val result6 = _6.transformWithValue(result5, values._6)
    _7.transformWithValue(result6, values._7)
  }
}
