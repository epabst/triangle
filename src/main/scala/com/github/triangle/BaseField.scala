package com.github.triangle

/** A trait for a typeless [[com.github.triangle.PortableField]] for convenience such as when defining a List of heterogeneous Fields. */
trait BaseField {
  // For use with when the Logging trait is mixed-in
  protected def logTag = "triangle"

  /**
   * Copies this field, the same as {{{copy(AnyRef,AnyRef)}}} except that
   * the copying from {{{from}}} happens immediately (on the current thread),
   * and the returned PortableValue can copy into the target on a separate thread, if desired..
   * It's a clean separation so that each step of the copy only accesses one of the objects.
   * @return a PortableValue that copies the value into its parameter
   */
  def copyFrom(from: AnyRef): PortableValue

  /**
   * Copies this field from the first applicable item in {{{fromItems}}}.
   * @return a PortableValue that copies the value into its parameter
   */
  def copyFromItem(fromItems: List[AnyRef]): PortableValue

  /**
   * Copies this field from {{{from}}} to {{{to}}}, if possible.
   */
  def copy(from: AnyRef, to: AnyRef) { copyFrom(from).copyTo(to, List(from)) }

  /**
   * Copies this field from the first applicable item in {{{fromItems}}} to {{{to}}}, if possible.
   */
  def copyFromItem(fromItems: List[AnyRef], to: AnyRef) { copyFromItem(fromItems).copyTo(to, fromItems) }

  /**
   * Transforms the {{{initial}}} subject using the {{{data}}} for this field..
   * @return the transformed subject, which could be the initial instance
   */
  def transform[S <: AnyRef](initial: S, data: AnyRef): S

  /**
   * Transforms the {{{initial}}} subject using the first applicable item in {{{dataItems}}} for this field..
   * @return the transformed subject, which could be the initial instance
   */
  def transformWithItem[S <: AnyRef](initial: S, dataItems: List[AnyRef]): S

  /**
   * Traverses all of the PortableFields in this PortableField, returning the desired information.
   * Anything not matched will be traversed deeper, if possible, or else ignored.
   * <pre>
   *   deepCollect {
   *     case foo: BarField => foo.myInfo
   *   }
   * </pre>
   */
  // Default implementation only checks this field.  This should be overridden for any field wrapping other fields.
  def deepCollect[R](f: PartialFunction[BaseField, R]): List[R] = f.lift(this).toList

  protected def from_to_for_field_message(from: AnyRef, to: AnyRef, field: BaseField): String =
    " from " + truncate(from) + " to " + truncate(to) + " for field " + truncate(field)

  protected def transform_with_forField_message(initial: AnyRef, data: Any, field: BaseField): String =
    "transform " + truncate(initial) + " with " + truncate(data) + " for field " + truncate(field)

  private[triangle] def truncate(any: Any): String = {
    val stripStrings = Option(any).collect { case ref: AnyRef => ref.getClass.getPackage.getName + "." }
    val rawString = String.valueOf(any)
    val string = stripStrings.foldLeft(rawString)((soFar, strip) => soFar.replace(strip, ""))
    string.substring(0, math.min(string.length, 25))
  }
}
