package com.github.triangle

/** A value that a PortableField operates on.
  * @author Eric Pabst (epabst@gmail.com)
  */
trait PortableValue {
  /** Copies this value to {{{to}}}, if possible.
    * @param contextItems a List of items that may be used by PortableField.setterUsingItems.
    */
  def copyTo(to: AnyRef, contextItems: List[AnyRef] = Nil)

  /** Transforms the {{{initial}}} subject using this value.
    * @param contextItems a List of items that may be used by PortableField.transformerUsingItems.
    * @return the transformed subject, which could be the initial instance
    */
  def transform[S <: AnyRef](initial: S, contextItems: List[AnyRef] = Nil): S

  /** Returns the value contained in this value for the given PortableField. */
  def get[T](field: PortableField[T]): Option[T]
}

class PortableValue1[T](field: PortableField[T], value: Option[T]) extends Tuple2(field, value) with PortableValue with Logging {
  protected def logTag = "triangle"

  def copyTo(to: AnyRef, contextItems: List[AnyRef] = Nil) {
    if (field.setterUsingItems.isDefinedAt((to, contextItems))) {
      copyToDefinedAt(to, contextItems)
    } else {
      debug("Unable to copy" + PortableField.from_to_for_field_message(value, to, field)  + " due to setter.")
    }
  }

  private def copyToDefinedAt(to: AnyRef, contextItems: List[AnyRef]) {
    trace("Copying " + value + PortableField.from_to_for_field_message(value, to, field))
    field.setterUsingItems((to, contextItems))(value)
  }

  def transform[S <: AnyRef](initial: S, contextItems: List[AnyRef] = Nil): S = {
    if (field.transformerUsingItems.isDefinedAt((initial, contextItems))) {
      trace("About to " + PortableField.transform_with_forField_message(initial, "value " + value, field))
      field.transformerUsingItems[S](initial, contextItems)(value)
    } else {
      debug("Unable to " + PortableField.transform_with_forField_message(initial, "value " + value, field) + " due to transformer")
      initial
    }
  }

  def get[T2](desiredField: PortableField[T2]): Option[T2] = if (field == desiredField) value.asInstanceOf[Option[T2]] else None

  override def toString = value.toString
}

class PortableValueSeq(portableValues: Traversable[PortableValue]) extends PortableValue with Logging {
  protected def logTag = "triangle"

  def copyTo(to: AnyRef, contextItems: List[AnyRef] = Nil) {
    debug("Copying PortableValue with " + portableValues + " to " + to)
    portableValues.foreach(_.copyTo(to, contextItems))
  }

  def transform[S <: AnyRef](initial: S, contextItems: List[AnyRef] = Nil): S = {
    debug("Transforming " + initial + " using PortableValue with " + portableValues)
    portableValues.foldLeft(initial)((subject, portableValue) => portableValue.transform(subject, contextItems))
  }

  def get[T](field: PortableField[T]): Option[T] = portableValues.view.flatMap(_.get(field)).headOption

  override def toString = "PortableValue(" + portableValues.mkString(",") + ")"
}

object PortableValue {
  def apply(portableValues: PortableValue1[_]*): PortableValue = new PortableValueSeq(portableValues)
}
