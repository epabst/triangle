package com.github.triangle

/** A trait for a typeless [[com.github.triangle.PortableField]] for convenience such as when defining a List of heterogeneous Fields. */
trait BaseField {
  lazy val defaultValue: PortableValue = copyFrom(PortableField.UseDefaults)

  // For use with when the Logging trait is mixed-in
  protected def logTag = "triangle"

  /** Copies this field, the same as {{{copy(AnyRef,AnyRef)}}} except that
    * the copying from {{{from}}} happens immediately (on the current thread),
    * and the returned PortableValue can copy into the target on a separate thread, if desired..
    * It's a clean separation so that each step of the copy only accesses one of the objects.
    * @return a PortableValue that copies the value into its parameter
    */
  def copyFrom(from: AnyRef): PortableValue

  /** Copies this field from the first applicable item in {{{fromItems}}}.
    * @return a PortableValue that copies the value into its parameter
    */
  def copyFrom(fromItems: GetterInput): PortableValue

  /** Copies this field from {{{from}}} to {{{to}}}, if possible. */
  def copy(from: AnyRef, to: AnyRef) { copy(GetterInput.single(from), to) }

  /** Copies this field from the first applicable item in {{{fromItems}}} to {{{to}}}, if possible. */
  def copy(fromItems: GetterInput, to: AnyRef) { copyFrom(fromItems).update(to, fromItems) }

  /**
   * Updates the {{{initial}}} subject using the {{{data}}} for this field..
   * @return the updated subject, which could be the initial instance
   */
  final def copyAndUpdate[S <: AnyRef](data: AnyRef, initial: S): S = {
    copyAndUpdate(GetterInput.single(data), initial)
  }

  /**
   * Updates the {{{initial}}} subject using the applicable items in {{{getterInput}}} for this field..
   * @return the updated subject, which could be the initial instance
   */
  def copyAndUpdate[S <: AnyRef](getterInput: GetterInput, initial: S): S

  /** Traverses all of the PortableFields in this PortableField, returning the desired information.
    * Anything not matched will be traversed deeper, if possible, or else ignored.
    * <pre>
    *   deepCollect {
    *     case foo: BarField => foo.myInfo
    *   }
    * </pre>
    */
  // Default implementation only checks this field.  This should be overridden for any field wrapping other fields.
  def deepCollect[R](f: PartialFunction[BaseField, R]): Seq[R] = f.lift(this).toSeq
}
