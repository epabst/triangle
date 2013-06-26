package com.github.triangle

/** A value that a PortableField operates on.
  * @author Eric Pabst (epabst@gmail.com)
  */
trait PortableValue {
  /**
   * Updates the {{{initial}}} subject using this value, if possible.
   * The new updated subject is returned.
   * Mutable subjects are modified in place as well as being returned.
   * @param context items that may be used by PortableField.updater.
   * @return the updated subject, which could be the initial instance
   */
  def update[S <: AnyRef](initial: S, context: GetterInput = GetterInput.empty): S = {
    update(UpdaterInput(initial, context))
  }

  /**
   * Update the {{{updaterInput.subject}}} with this value.
   * The new updated subject is returned.
   * Mutable subjects are modified in place as well as being returned.
   * @param updaterInput an UpdaterInput that doesn't have a value.
   *                     This is because the value is not needed since it's already in the PortableValue.
   */
  def update[S <: AnyRef](updaterInput: UpdaterInput[S, Nothing]): S

  /** Returns the value contained in this value for the given PortableField. */
  def get[T](field: PortableField[T]): Option[T]
}

class PortableValue1[T](field: PortableField[T], value: Option[T]) extends Tuple2(field, value) with PortableValue with Logging {
  protected def logTag = "triangle"

  /**
   * Update the {{{updaterInput.subject}}} with this value.
   * @param updaterInput an UpdaterInput that doesn't have a value.
   *                     This is because the value is not needed since it's already in the PortableValue.
   */
  def update[S <: AnyRef](updaterInput: UpdaterInput[S, Nothing]): S = {
    val initial = updaterInput.subject
    val updaterInputWithValue = updaterInput.withValue(value)
    trace("About to attempt to " + PortableField.update_with_forField_message(initial, "value " + value, field))
    field.updaterVal.attempt(updaterInputWithValue).getOrElse {
      debug("Unable to " + PortableField.update_with_forField_message(initial, "value " + value, field) + " due to updater")
      initial
    }
  }

  def get[T2](desiredField: PortableField[T2]): Option[T2] = if (field == desiredField) value.asInstanceOf[Option[T2]] else None

  override def toString() = value.toString
}

case class PortableValueSeq(portableValues: Traversable[PortableValue]) extends PortableValue with Logging {
  protected def logTag = "triangle"

  def update[S <: AnyRef](updaterInput: UpdaterInput[S, Nothing]): S = {
    val initial = updaterInput.subject
    debug("Updating " + initial + " using PortableValue with " + portableValues)
    portableValues.foldLeft(initial)((subject, portableValue) => portableValue.update(updaterInput.copy(subject = subject)))
  }

  def get[T](field: PortableField[T]): Option[T] = portableValues.view.flatMap(_.get(field)).headOption

  override lazy val toString = "PortableValueSeq(" + portableValues.mkString(",") + ")"
}

object PortableValue {
  lazy val empty = new PortableValueSeq(Traversable.empty) {
    override lazy val toString = "PortableValue.empty"
  }

  def apply(portableValues: PortableValue1[_]*): PortableValue = new PortableValueSeq(portableValues)
}
