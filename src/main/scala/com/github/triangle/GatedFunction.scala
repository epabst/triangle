package com.github.triangle

/**
 * A PartialFunct for which the isDefinedAt is defined separately fom the applyToDomain method
 * for ease when implementing a PartialFunct.
 * This can be less efficient than extending PartialFunct directly since some of the isDefinedAt calculation
 * can be re-used in PartialFunct but cannot be used by here.
 * @author Eric Pabst (epabst@gmail.com)
 *         Date: 6/25/13
 *         Time: 6:00 PM
 */
abstract class GatedFunction[-A,+B] extends PartialFunct[A,B] {
  final def attempt(x: A) = if (isDefinedAt(x)) Some(applyToDomain(x)) else None

  /** Apply this function to an input which is guaranteed to cause isDefinedAt to return true.
    * It should not even check if such is the case.  The results are totally undefined if not the case.
    * @param x the input to the function for which isDefinedAt(x) is guaranteed to be true
    * @return the result of the function
    */
  def applyToDomain(x: A): B
}
