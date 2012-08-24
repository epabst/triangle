package com.github.triangle

/** A value that a PortableField operates on.
  * @author Eric Pabst (epabst@gmail.com)
  */
trait PortableValue {
  /** Copies this value to {{{to}}}, if possible.
    * @param contextItems a List of items that may be used by PortableField.setterUsingItems.
    */
  def copyTo(to: AnyRef, contextItems: GetterInput = GetterInput.empty)

  /**
   * Updates the {{{initial}}} subject using this value.
   * @param context items that may be used by PortableField.transformerUsingItems.
   * @return the transformed subject, which could be the initial instance
   */
  def update[S <: AnyRef](initial: S, context: GetterInput = GetterInput.empty): S

  /** Returns the value contained in this value for the given PortableField. */
  def get[T](field: PortableField[T]): Option[T]
}

class PortableValue1[T](field: PortableField[T], value: Option[T]) extends Tuple2(field, value) with PortableValue with Logging {
  protected def logTag = "triangle"

  @deprecated("use copyTo(to, GetterInput(contextItems))")
  def copyTo(to: AnyRef, contextItems: List[AnyRef]) {
    copyTo(to, GetterInput(contextItems))
  }

  def copyTo(to: AnyRef, getterInput: GetterInput = GetterInput.empty) {
    copyTo(UpdaterInput(to, getterInput))
  }

  /**
   * Copy this value into {{{updaterInput.subject}}}.  This is identical to update(updaterInput) but may be more readable.
   * @param updaterInput an UpdaterInput that doesn't have a value.
   *                     This is because the value is not needed since it's already in the PortableValue.
   */
  def copyTo[S <: AnyRef](updaterInput: UpdaterInput[S, Nothing]) {
    update(updaterInput)
  }

  /** Update the {{{initial}}} with this value. */
  def update[S <: AnyRef](initial: S, getterInput: GetterInput = GetterInput.empty): S = {
    update(UpdaterInput(initial, getterInput))
  }

  /**
   * Update the {{{updaterInput.subject}}} with this value.  This is identical to copyTo(updaterInput) but may be more readable.
   * @param updaterInput an UpdaterInput that doesn't have a value.
   *                     This is because the value is not needed since it's already in the PortableValue.
   */
  def update[S <: AnyRef](updaterInput: UpdaterInput[S, Nothing]): S = {
    val initial = updaterInput.subject
    val updaterInputWithValue = updaterInput.withValue(value)
    if (field.updater.isDefinedAt(updaterInputWithValue)) {
      trace("About to " + PortableField.update_with_forField_message(initial, "value " + value, field))
      field.updater(updaterInputWithValue)
    } else {
      debug("Unable to " + PortableField.update_with_forField_message(initial, "value " + value, field) + " due to updater")
      initial
    }
  }

  def get[T2](desiredField: PortableField[T2]): Option[T2] = if (field == desiredField) value.asInstanceOf[Option[T2]] else None

  override def toString() = value.toString
}

class PortableValueSeq(portableValues: Traversable[PortableValue]) extends PortableValue with Logging {
  protected def logTag = "triangle"

  def copyTo(to: AnyRef, contextItems: GetterInput = GetterInput.empty) {
    debug("Copying PortableValue with " + portableValues + " to " + to)
    portableValues.foreach(_.copyTo(to, contextItems))
  }

  def update[S <: AnyRef](initial: S, contextItems: GetterInput = GetterInput.empty): S = {
    debug("Transforming " + initial + " using PortableValue with " + portableValues)
    portableValues.foldLeft(initial)((subject, portableValue) => portableValue.update(subject, contextItems))
  }

  def get[T](field: PortableField[T]): Option[T] = portableValues.view.flatMap(_.get(field)).headOption

  override def toString = "PortableValue(" + portableValues.mkString(",") + ")"
}

object PortableValue {
  def apply(portableValues: PortableValue1[_]*): PortableValue = new PortableValueSeq(portableValues)
}
