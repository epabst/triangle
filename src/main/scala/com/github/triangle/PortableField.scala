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
  def ->(value: Option[T]): PortableValue1[T] = new PortableValue1[T](this, value)

  def ->(value: T): PortableValue1[T] = ->(Some(value))

  /** PartialFunction for getting an optional value from the first AnyRef in the GetterInput that has Some value.
    * If none of them has Some value, then it will return None if at least one of them applies.
    * If none of them even apply, the PartialFunction won't match at all (i.e. isDefinedAt will be false).
    */
  def getter: PartialFunction[GetterInput,Option[T]]

  /**
   * Get an optional value from the given AnyRef.
   * @throws MatchError if this field is undefined for the given AnyRef
   */
  def getValue(readable: AnyRef): Option[T] = {
    val result = getter(GetterInput.single(readable))
    require(result != null, this + "'s getter is non-functional.  It should never return a null.")
    result
  }

  /**
   * Get an optional value from the given AnyRef.
   * This is here for convenience as an alias for {{{getValue}}}, although that may be more readable.
   * @throws MatchError if this field is undefined for the given AnyRef
   */
  def apply(readable: AnyRef): Option[T] = getValue(readable)

  /** Gets the value, similar to {{{Map.apply}}}, and the value must not be None.
    * @see [[com.github.triangle.PortableField.getter]]
    * @return the value
    * @throws NoSuchElementException if the value was None
    * @throws MatchError if subject is not an applicable type
    */
  def getRequired(subject: AnyRef): T = getValue(subject).get

  /*
   * An extractor from a GetterInput that matches the value as an Option.
   * Example: {{{
   * case MyField(Some(string)) => ...
   * case MyField(None) => ...
   * }}}
   */
  def unapply(subject: GetterInput): Option[Option[T]] = subject match {
    case items: GetterInput if getter.isDefinedAt(items) => Some(getter(items))
    case _ => None
  }

  /** An extractor from an AnyRef that matches the value as an Option.
    * Example: {{{case MyField(Some(string)) => ...}}}
    */
  def unapply(subject: AnyRef): Option[Option[T]] = unapply(GetterInput.single(subject))

  /** Updates the {{{initial}}} subject using the {{{value}}} for this field and some context.
    * @return the updated subject, which could be the initial instance
    */
  def updateWithValue[S <: AnyRef](initial: S, value: Option[T], context: GetterInput = GetterInput.empty): S = {
    update(UpdaterInput(initial, value, context))
  }

  /**
   * Updates {{{updaterInput}}} for this field.
   * @return the updated subject, which could be the initial instance
   */
  def update[S <: AnyRef](updaterInput: UpdaterInput[S,T]): S = {
    val defined = updater[S].isDefinedAt(updaterInput)
    if (defined) {
      updater(updaterInput)
    } else {
      debug("Unable to update " + updaterInput.subject + " with value " + updaterInput.valueOpt + " for field " + this + " with context " + updaterInput.context + ".")
      updaterInput.subject
    }
  }

  /*
   * PartialFunction for updating an AnyRef using an optional value and context.
   * {{{updater(UpdaterInput(foo, valueOpt, GetterInput(...))}}} should return an updated version of foo
   * (which could be the same instance if mutable).
   * Note: Implementations usually must specify the return type to compile properly.
   * The parameter's subject is what will be updated, whether immutable or mutable.
   * The return value is ignored if the subject is mutable (and presumably updated in place).
   */
  def updater[S <: AnyRef]: PartialFunction[UpdaterInput[S,T],S]

  def isUpdaterDefinedAt(input: UpdaterInput[_ <: AnyRef, Nothing]): Boolean = updater.isDefinedAt(input)

  //inherited
  def copyAndUpdate[S <: AnyRef](input: GetterInput, initial: S): S = {
    if (updater.isDefinedAt(UpdaterInput(initial, input))) {
      copyFrom(input).update(initial, input)
    } else {
      debug("Unable to " + PortableField.update_with_forField_message(initial, input, this) + " because of updater.")
      initial
    }
  }

  def copyFrom(from: AnyRef): PortableValue1[T] = copyFrom(GetterInput.single(from))

  def copyFrom(input: GetterInput): PortableValue1[T] =
    this -> (if (getter.isDefinedAt(input)) getter(input) else None)

  override def copy(from: AnyRef, to: AnyRef) {
    copy(GetterInput.single(from), to)
  }

  //inherited
  override def copy(input: GetterInput, to: AnyRef) {
    if (updater.isDefinedAt(UpdaterInput(to, input))) {
      copyFrom(input).update(to, input)
    } else {
      debug("Unable to copy" + PortableField.from_to_for_field_message(input, to, this)  + " due to setter.")
    }
  }

  /** Adds two PortableField objects together. */
  def +(other: PortableField[T]): PortableField[T] = TypedFieldSeq(self, other)
}

/**
 * A VERY useful extractor that joins two other extractors and requires that both succeed. *
 * Usage: {{{
 *   case MyField1(value1Opt) && MyField2(value2Opt) => ...
 * }}}
 */
case object && { def unapply[A](a: A) = Some((a, a))}

/** Factory methods for basic PortableFields.  This should be imported as PortableField._. */
object PortableField {
  def emptyField[T]: PortableField[T] = new NoGetter[T] with NoUpdater[T]

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

  /** A common function for the second parameter such as <code>Updater[S,T](..., noUpdaterForEmpty)</code>. */
  def noUpdaterForEmpty[S]: S => S = {s => s}

  private sealed class UseDefaults
  val UseDefaults: AnyRef = new UseDefaults

  /** Defines a default for a field value, used when copied from UseDefaults. */
  def default[T](value: => T): PortableField[T] = new SingleGetter[T] {
    def singleGetter = { case _: UseDefaults => Some(value) }

    override def toString = "default(" + value + ")"
  }

  def mapFieldWithKey[T,K](key: K): PortableField[T] = new DelegatingPortableField[T] {
    val delegate = Getter[collection.Map[K,_ <: T],T](_.get(key)) +
      Updater((m: Map[K,_ >: T]) => (value: T) => m + (key -> value), (m: Map[K,_ >: T]) => m - key) +
      Setter((m: mutable.Map[K,_ >: T]) => (v: T) => m.put(key, v), (m: mutable.Map[K,_ >: T]) => m.remove(key))

    override def toString = "mapField(" + key + ")"
  }

  def mapField[T](name: String): PortableField[T] = mapFieldWithKey[T,String](name)

  /** Adjusts the subject if it is of the given type and if Unit is provided as one of the items to copy from. */
  def adjustmentInPlace[S <: AnyRef](adjuster: S => Unit)(implicit subjectManifest: ClassManifest[S]): PortableField[Unit] =
    default[Unit](Unit) + Setter((s: S) => u => adjuster(s))

  /** Adjusts the subject if it is of the given type and if Unit is provided as one of the items to copy from. */
  def adjustment[S <: AnyRef](adjuster: S => S)(implicit subjectManifest: ClassManifest[S]): PortableField[Unit] =
    default[Unit](Unit) + Updater((s: S) => (u: Option[Unit]) => adjuster(s))

  def converted[A,B](converter1: Converter[A,B], field: PortableField[B], converter2: Converter[B,A]): PortableField[A] =
    new ConvertedField[A,B](field) {
      def convert(value: B) = converter2.convert(value)

      def unconvert(value: A) = converter1.convert(value)
    }

  def formatted[T](format: ValueFormat[T], field: PortableField[String]) = new FormattedField(format, field)

  /** formatted replacement for primitive values. */
  def formatted[T <: AnyVal](field: PortableField[String])(implicit m: Manifest[T]): PortableField[T] =
    formatted(ValueFormat.basicFormat[T], field)

  private[triangle] def from_to_for_field_message(from: AnyRef, to: AnyRef, field: BaseField): String =
    " from " + truncate(from) + " to " + truncate(to) + " for field " + truncate(field)

  private[triangle] def update_with_forField_message(initial: AnyRef, data: Any, field: BaseField): String =
    "update " + truncate(initial) + " with " + truncate(data) + " for field " + truncate(field)

  private[triangle] def truncate(any: Any): String = {
    val stripStrings = Option(any).collect { case ref: AnyRef => ref.getClass.getPackage.getName + "." }
    val rawString = String.valueOf(any)
    val string = stripStrings.foldLeft(rawString)((soFar, strip) => soFar.replace(strip, ""))
    string.substring(0, math.min(string.length, 25))
  }
}
