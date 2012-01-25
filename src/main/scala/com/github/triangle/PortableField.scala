package com.github.triangle

import collection.mutable

/** A portable field of a specific type which applies to Cursors, Views, Model objects, etc.
  * Example:
  * {{{
  * import com.github.triangle.PortableField._
  * import com.github.scala.android.crud.persistence.CursorField._
  * import com.github.scala.android.crud.persistence.PersistedType._
  * import com.github.scala.android.crud.view.ViewField._
  *
  * val fields = List(
  *   persisted[String]("name") + viewId(R.id.name, textView),
  *   persisted[Double]("score") + viewId(R.id.score, formatted[Double](textView))
  * )
  * }}}
  * Usage of implicits and defaults make this syntax concise for the simple cases,
  * but allow for very complex situations as well by providing explicit values when needed.
  * T is the value type that this PortableField gets and sets.
  * @see #getter
  * @see #setter
  */
trait PortableField[T] extends BaseField with Logging { self =>
  /** PartialFunction for getting an optional value from an AnyRef. */
  def getter: PartialFunction[AnyRef,Option[T]]

  /** extractor for finding the applicable items, if any. */
  private object ApplicableItems {
    def unapply(items: List[_]): Option[List[AnyRef]] = {
      val applicableItems = items.map(_.asInstanceOf[AnyRef]).filter(getter.isDefinedAt(_))
      if (applicableItems.isEmpty) None else Some(applicableItems)
    }
  }

  private def get(readable: AnyRef): Option[T] = {
    val result = getter(readable)
    require(result != null, this + "'s getter is non-functional.  It should never return a null.")
    result
  }

  /** PartialFunction for getting an optional value from the first AnyRef in the List that has Some value.
    * If none of them has Some value, then it will return None if at least one of them applies.
    * If none of them even apply, the PartialFunction won't match at all (i.e. isDefinedAt will be false).
    */
  def getterFromItem: PartialFunction[List[_],Option[T]] = {
    case ApplicableItems(items) => items.view.map(get(_)).find(_.isDefined).getOrElse(None)
  }

  /** Gets the value, similar to {{{Map.apply}}}, and the value must not be None.
    * @see [[com.github.triangle.PortableField.getter]]
    * @return the value
    * @throws NoSuchElementException if the value was None
    * @throws MatchError if subject is not an applicable type
    */
  def apply(subject: AnyRef): T = get(subject).get

  /** An extractor that matches the value as an Option.
    * Example: {{{case MyField(Some(string)) => ...}}}
    */
  def unapply(subject: AnyRef): Option[Option[T]] = subject match {
    case x if getter.isDefinedAt(x) => Some(get(x))
    case items: List[_] if getterFromItem.isDefinedAt(items) => Some(getterFromItem(items))
    case _ => None
  }

  /** PartialFunction for setting an optional value in an AnyRef. */
  def setter: PartialFunction[AnyRef,Option[T] => Unit]

  /** A setter that also has access to some items such as context that may be helpful when setting. */
  def setterUsingItems: PartialFunction[(AnyRef,List[AnyRef]),Option[T] => Unit] = {
    case (subject, _) if setter.isDefinedAt(subject) => v => setter(subject)(v)
  }

  /** Sets a value in {{{subject}}} by using all embedded PortableFields that can handle it.
    * @param items optional extra items usable by the setterUsingItems
    * @return true if any were successful
    */
  def setValue(subject: AnyRef, value: Option[T], items: List[AnyRef] = Nil): Boolean = {
    val defined = setterUsingItems.isDefinedAt(subject, items)
    if (defined) setterUsingItems(subject, items)(value)
    else debug("Unable to set value " + value + " into " + subject + " for field " + this + " with items " + items + ".")
    defined
  }

  /** Transforms the {{{initial}}} subject using the {{{data}}} for this field..
    * @return the transformed subject, which could be the initial instance
    */
  def transformWithValue[S <: AnyRef](initial: S, value: Option[T]): S = transformer[S](initial)(value)

  /** PartialFunction for transforming an AnyRef using an optional value.
    * This delegates to {{{setter}}} for mutable objects.
    * {{{transformer(foo)(value)}}} should return a transformed version of foo (which could be the same instance if mutable).
    * Note: Implementations usually must specify the return type to compile properly
    * The parameter is the subject to be transformed, whether immutable or mutable
    */
  def transformer[S <: AnyRef]: PartialFunction[S,Option[T] => S]

  /** A transformer that also has access to some items such as context that may be helpful when transforming. */
  def transformerUsingItems[S <: AnyRef]: PartialFunction[(S,List[AnyRef]),Option[T] => S] = {
    case (subject, _) if transformer.isDefinedAt(subject) => v => transformer[S](subject)(v)
  }

  private def transformUsingGetFunctionCheckingTransformerFirst[S <: AnyRef,F <: AnyRef](initialAndItems: (S,List[AnyRef]),
                                                                                         get: PartialFunction[F,Option[T]],
                                                                                         data: F): S = {
    val initial = initialAndItems._1
    if (transformerUsingItems.isDefinedAt(initialAndItems)) {
      copyFromUsingGetFunction(get, data).transform(initial, initialAndItems._2)
    } else {
      debug("Unable to " + transform_with_forField_message(initial, data, this) + " because of transformer.")
      initial
    }
  }

  //inherited
  def transform[S <: AnyRef](initial: S, data: AnyRef): S = {
    transformUsingGetFunctionCheckingTransformerFirst[S,AnyRef]((initial, List(data)), getter, data)
  }

  //inherited
  def transformWithItem[S <: AnyRef](initial: S, dataItems: List[AnyRef]): S =
    transformUsingGetFunctionCheckingTransformerFirst[S,List[AnyRef]]((initial, dataItems), getterFromItem, dataItems)

  def copyFrom(from: AnyRef) = copyFromUsingGetFunction(getter, from)

  def copyFromItem(fromItems: List[AnyRef]) = copyFromUsingGetFunction(getterFromItem, fromItems)

  override def copy(from: AnyRef, to: AnyRef) {
    copyUsingGetFunctionCheckingSetterFirst(getter, from, (to, List(from)))
  }

  //inherited
  override def copyFromItem(fromItems: List[AnyRef], to: AnyRef) {
    copyUsingGetFunctionCheckingSetterFirst(getterFromItem, fromItems, (to, fromItems))
  }

  private def copyUsingGetFunctionCheckingSetterFirst[F <: AnyRef](get: PartialFunction[F,Option[T]], from: F, toAndItems: (AnyRef,List[AnyRef])) {
    if (setterUsingItems.isDefinedAt(toAndItems)) {
      copyFromUsingGetFunction(get, from).copyTo(toAndItems._1, toAndItems._2)
    } else {
      debug("Unable to copy" + from_to_for_field_message(from, toAndItems._1, this)  + " due to setter.")
    }
  }

  private def copyFromUsingGetFunction[F <: AnyRef](getFunction: PartialFunction[F,Option[T]], from: F): PortableValue = {
    val value: Option[T] = if (getFunction.isDefinedAt(from)) getFunction(from) else None
    new PortableValue {
      def copyTo(to: AnyRef, contextItems: List[AnyRef] = Nil) {
        if (setterUsingItems.isDefinedAt((to, contextItems))) {
          copyToDefinedAt(to, contextItems)
        }
      }

      private def copyToDefinedAt(to: AnyRef, contextItems: List[AnyRef]) {
        debug("Copying " + value + from_to_for_field_message(from, to, self))
        setterUsingItems((to, contextItems))(value)
      }

      def transform[S <: AnyRef](initial: S, contextItems: List[AnyRef] = Nil): S = {
        debug("About to " + transform_with_forField_message(initial, "value " + value, self))
        if (transformerUsingItems.isDefinedAt((initial, contextItems))) {
          transformerUsingItems[S](initial, contextItems)(value)
        } else {
          initial
        }
      }

      def get[T2](field: PortableField[T2]): Option[T2] = if (self == field) value.asInstanceOf[Option[T2]] else None

      override def toString = value.toString
    }
  }

  /** Adds two PortableField objects together. */
  def +(other: PortableField[T]): PortableField[T] = {
    new PortableField[T] {
      override def toString = self + " + " + other

      def getter = {
        case x if self.getter.isDefinedAt(x) || other.getter.isDefinedAt(x) => {
          val values = List(self, other).view.collect {
            case field if field.getter.isDefinedAt(x) => field.get(x)
          }
          values.find(_.isDefined).getOrElse(None)
        }
      }

      override def getterFromItem = {
        case items if self.getterFromItem.isDefinedAt(items) || other.getterFromItem.isDefinedAt(items) => {
          val values = List(self, other).view.map(_.getterFromItem).filter(_.isDefinedAt(items)).map(_(items))
          values.find(_.isDefined).getOrElse(None)
        }
      }

      /** Combines the two setters, calling only applicable ones (not just the first though). */
      def setter: PartialFunction[AnyRef,Option[T] => Unit] = {
        case x if self.setter.isDefinedAt(x) || other.setter.isDefinedAt(x) => { value =>
          val definedFields = List(self, other).view.filter(_.setter.isDefinedAt(x))
          if (definedFields.isEmpty) throw new MatchError("setter in " + PortableField.this)
          else definedFields.foreach(_.setter(x)(value))
        }
      }

      /** Combines the two setterUsingItems, calling only applicable ones (not just the first though). */
      override def setterUsingItems: PartialFunction[(AnyRef,List[AnyRef]),Option[T] => Unit] = {
        case x if self.setterUsingItems.isDefinedAt(x) || other.setterUsingItems.isDefinedAt(x) => { value =>
          val definedFields = List(self, other).view.filter(_.setterUsingItems.isDefinedAt(x))
          if (definedFields.isEmpty) throw new MatchError("setterUsingItems in " + PortableField.this)
          else definedFields.foreach(_.setterUsingItems(x)(value))
        }
      }

      def transformer[S <: AnyRef] = {
        case subject if self.transformer.isDefinedAt(subject) || other.transformer.isDefinedAt(subject) => { value =>
          val definedFields = List(self, other).filter(_.transformer.isDefinedAt(subject))
          definedFields.foldLeft(subject)((subject, field) => field.transformer[S](subject)(value))
        }
      }

      override def transformerUsingItems[S <: AnyRef]: PartialFunction[(S,List[AnyRef]),Option[T] => S] = {
        case x if self.transformerUsingItems.isDefinedAt(x) || other.transformerUsingItems.isDefinedAt(x) => { value =>
          val definedFields = List(self, other).filter(_.transformerUsingItems.isDefinedAt(x))
          definedFields.foldLeft(x)((soFar, field) => (field.transformerUsingItems[S](soFar)(value), soFar._2))._1
        }
      }

      override def deepCollect[R](f: PartialFunction[BaseField, R]): List[R] = {
        super.deepCollect[R](f) match {
          case Nil =>
            val lifted = f.lift
            List(self, other).flatMap(field => lifted(field).map(List[R](_)).getOrElse(field.deepCollect(f)))
          case x => x
        }
      }
    }
  }
}
case object && { def unapply[A](a: A) = Some((a, a))}

/** Factory methods for basic PortableFields.  This should be imported as PortableField._. */
object PortableField {
  def emptyField[T]: PortableField[T] = new NoGetter[T] with NoTransformer[T]

  private[triangle] def emptyPartialFunction[A,B] = new PartialFunction[A,B] {
    def isDefinedAt(x: A) = false

    def apply(v1: A) = throw new MatchError("emptyPartialFunction")
  }

  //This is here so that getters can be written more simply by not having to explicitly wrap the result in a "Some".
  implicit def toSome[T](value: T): Option[T] = Some(value)

  /** Defines a read-only field for returning the subject item itself (as an Option). */
  def identityField[S <: AnyRef](implicit subjectManifest: ClassManifest[S]) = new DelegatingPortableField[S] {
    val delegate = Getter[S,S](subject => Some(subject))

    override def toString = "identifyField[" + subjectManifest.erasure.getSimpleName + "]"
  }

  /** A common function for the second parameter such as <code>Setter[S,T](..., noSetterForEmpty)</code>. */
  def noSetterForEmpty[S]: S => Unit = {_: S => }

  /** A common function for the second parameter such as <code>Transformer[S,T](..., noTransformerForEmpty)</code>. */
  def noTransformerForEmpty[S]: S => S = {s => s}

  private sealed class UseDefaults
  val UseDefaults: AnyRef = new UseDefaults

  /** Defines a default for a field value, used when copied from UseDefaults. */
  def default[T](value: => T): PortableField[T] = new Getter[T] {
    def getter = { case _: UseDefaults => Some(value) }

    override def toString = "default(" + value + ")"
  }

  def mapFieldWithKey[T,K](key: K): PortableField[T] = new DelegatingPortableField[T] {
    val delegate = Getter[collection.Map[K,_ <: T],T](_.get(key)) +
      Transformer((m: Map[K,_ >: T]) => (value: T) => m + (key -> value), (m: Map[K,_ >: T]) => m - key) +
      Setter((m: mutable.Map[K,_ >: T]) => (v: T) => m.put(key, v), (m: mutable.Map[K,_ >: T]) => m.remove(key))

    override def toString = "mapField(" + key + ")"
  }

  def mapField[T](name: String): PortableField[T] = mapFieldWithKey[T,String](name)

  /** Adjusts the subject if it is of the given type and if Unit is provided as one of the items to copy from. */
  def adjustmentInPlace[S <: AnyRef](adjuster: S => Unit)(implicit subjectManifest: ClassManifest[S]): PortableField[Unit] =
    default[Unit](Unit) + Setter((s: S) => u => adjuster(s))

  /** Adjusts the subject if it is of the given type and if Unit is provided as one of the items to copy from. */
  def adjustment[S <: AnyRef](adjuster: S => S)(implicit subjectManifest: ClassManifest[S]): PortableField[Unit] =
    default[Unit](Unit) + Transformer((s: S) => (u: Option[Unit]) => adjuster(s))

  def converted[A,B](converter1: Converter[A,B], field: PortableField[B], converter2: Converter[B,A]): PortableField[A] =
    new ConvertedField[A,B](field) {
      def convert(value: B) = converter2.convert(value)

      def unconvert(value: A) = converter1.convert(value)
    }

  def formatted[T](format: ValueFormat[T], field: PortableField[String]) = new FormattedField(format, field)

  /** formatted replacement for primitive values. */
  def formatted[T <: AnyVal](field: PortableField[String])(implicit m: Manifest[T]): PortableField[T] =
    formatted(ValueFormat.basicFormat[T], field)
}
