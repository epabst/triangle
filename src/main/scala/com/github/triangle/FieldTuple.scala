package com.github.triangle

/**
 * A Tuple of Fields.
 * Use the right subtype for the correct -arity such as FieldTuple3.
 * @author Eric Pabst (epabst@gmail.com)
 * Date: 5/20/11
 * Time: 9:50 AM
 */
trait FieldTuple extends TypedProduct[PortableField[_]] {
  /** a type which is a Tuple for the field values such as (Option[A], Option[B], Option[C]). */
  type ValuesTuple <: Product

  /** Gets a Tuple with the results of calling each getter with {{{readable}}} as the parameter. */
  def valuesTuple(readable: AnyRef): ValuesTuple

  /** Gets a Tuple with the results of calling each getterFromItem with {{{items}}} as the parameter. */
  def valuesTupleFromItem(items: List[AnyRef]): ValuesTuple

  /** Sets the values into {{{subject}}}. */
  def setValues[T](subject: AnyRef, values: ValuesTuple)

  //this is only here to help the IDE to infer the type concretely
  override def productIterator: Iterator[PortableField[_]] = super.productIterator

  /** Creates a Setter PortableField that accepts a composite type T and a splitter function. */
  def setter[T](splitter: T => ValuesTuple): Setter[T] = {
    new Setter[T] {
      def setter = {
        case ref if productIterator.forall(_.setter.isDefinedAt(ref)) => {
          case Some(compositeValue) =>
            setValues(ref, splitter(compositeValue))
          case None =>
            productIterator.foreach(_.setValue(ref, None))
        }
      }

      override def deepCollect[R](f: PartialFunction[BaseField, R]): List[R] = {
        val lifted = f.lift
        productIterator.toList.flatMap(field => lifted(field).map(List(_)).getOrElse(field.deepCollect(f)))
      }
    }
  }

  def canEqual(that: Any) = that match {
    case x: AnyRef => this.getClass == x.getClass
    case _ => false
  }
}

/** The implicit toTupleXOfSomes defs are useful when defining a setter for a FieldTuple. */
object FieldTuple {
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
  def valuesTuple(readable: AnyRef) = (_1.getter(readable), _2.getter(readable))
  def valuesTupleFromItem(items: List[AnyRef]) = (_1.getterFromItem(items), _2.getterFromItem(items))
  def setValues[T](subject: AnyRef, values: ValuesTuple) {
    _1.setter(subject)(values._1)
    _2.setter(subject)(values._2)
  }
}

case class FieldTuple3[F1,F2,F3](_1: PortableField[F1], _2: PortableField[F2], _3: PortableField[F3])
        extends FieldTuple with Product3[PortableField[F1],PortableField[F2],PortableField[F3]] {
  type ValuesTuple = (Option[F1], Option[F2], Option[F3])
  def valuesTuple(readable: AnyRef) = (_1.getter(readable), _2.getter(readable), _3.getter(readable))
  def valuesTupleFromItem(items: List[AnyRef]) = (_1.getterFromItem(items), _2.getterFromItem(items), _3.getterFromItem(items))
  def setValues[T](subject: AnyRef, values: ValuesTuple) {
    _1.setter(subject)(values._1)
    _2.setter(subject)(values._2)
    _3.setter(subject)(values._3)
  }
}

case class FieldTuple4[F1,F2,F3,F4](_1: PortableField[F1], _2: PortableField[F2], _3: PortableField[F3],
                                    _4: PortableField[F4])
        extends FieldTuple with Product4[PortableField[F1],PortableField[F2],PortableField[F3],PortableField[F4]] {
  type ValuesTuple = (Option[F1], Option[F2], Option[F3], Option[F4])
  def valuesTuple(readable: AnyRef) = (_1.getter(readable), _2.getter(readable), _3.getter(readable), _4.getter(readable))
  def valuesTupleFromItem(items: List[AnyRef]) = (_1.getterFromItem(items), _2.getterFromItem(items), _3.getterFromItem(items),
          _4.getterFromItem(items))
  def setValues[T](subject: AnyRef, values: ValuesTuple) {
    _1.setter(subject)(values._1)
    _2.setter(subject)(values._2)
    _3.setter(subject)(values._3)
    _4.setter(subject)(values._4)
  }
}

case class FieldTuple5[F1,F2,F3,F4,F5](_1: PortableField[F1], _2: PortableField[F2], _3: PortableField[F3],
                                       _4: PortableField[F4], _5: PortableField[F5])
        extends FieldTuple with Product5[PortableField[F1],PortableField[F2],PortableField[F3],PortableField[F4],PortableField[F5]] {
  type ValuesTuple = (Option[F1], Option[F2], Option[F3], Option[F4], Option[F5])
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
}

case class FieldTuple6[F1,F2,F3,F4,F5,F6](_1: PortableField[F1], _2: PortableField[F2], _3: PortableField[F3],
                                          _4: PortableField[F4], _5: PortableField[F5], _6: PortableField[F6])
        extends FieldTuple with Product6[PortableField[F1],PortableField[F2],PortableField[F3],PortableField[F4],PortableField[F5],PortableField[F6]] {
  type ValuesTuple = (Option[F1], Option[F2], Option[F3], Option[F4], Option[F5], Option[F6])
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
}

case class FieldTuple7[F1,F2,F3,F4,F5,F6,F7](_1: PortableField[F1], _2: PortableField[F2], _3: PortableField[F3],
                                             _4: PortableField[F4], _5: PortableField[F5], _6: PortableField[F6],
                                             _7: PortableField[F7])
        extends FieldTuple with Product7[PortableField[F1],PortableField[F2],PortableField[F3],PortableField[F4],PortableField[F5],PortableField[F6],PortableField[F7]] {
  type ValuesTuple = (Option[F1], Option[F2], Option[F3], Option[F4], Option[F5], Option[F6], Option[F7])
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
}
