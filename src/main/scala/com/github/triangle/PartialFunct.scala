package com.github.triangle

/**
 * A wrapper around PartialFunction to make it perform better.
 * It has an issue described by a maintainer of "unfiltered" where the apply method calls the isDefinedAt method.
 * This is a problem because the contract is that apply shouldn't be called if isDefinedAt returns false,
 * however, it's run in apply anyway, so all the work that it does including unapply method calls, conditions, etc.
 * is duplicated.  This is especially a problem as PartialFunctions are composed such as using the orElse method
 * since the problem multiplies as O(N-squared) performance where N is the number of PartialFunctions composed together.
 *@author Eric Pabst (epabst@gmail.com)
 *         Date: 6/21/13
 *         Time: 4:55 PM
 */
abstract class PartialFunct[-A,+B] extends PartialFunction[A,B] { self =>
  override val lift = attempt(_)

  def attempt(x: A): Option[B]

  def attemptAndCheckForNulls(x: A): Option[B] = {
    val attemptedValueOpt = attempt(x)
    require(attemptedValueOpt != null, this + " is non-functional.  'attempt' should never return a null.")
    require(attemptedValueOpt != Some(null), this + " is non-functional.  'attempt' should never return a Some(null).")
    attemptedValueOpt
  }

  final def apply(x: A) = attempt(x).getOrElse(throw new MatchError(x + " (in function=" + this + ")"))

  override def andThen[C](f: (B) => C): PartialFunct[A, C] = {
    new PartialFunct[A, C] {
      def isDefinedAt(x: A) = self.isDefinedAt(x)

      def attempt(x: A) = self.attempt(x).map(f)
    }
  }

  override def orElse[A1 <: A, B1 >: B](that: PartialFunction[A1, B1]): PartialFunct[A1, B1] = {
    val thatFunct = PartialFunct(that)
    new PartialFunct[A1,B1] {
      def isDefinedAt(x: A1) = self.isDefinedAt(x) || that.isDefinedAt(x)

      def attempt(x: A1) = self.attempt(x).orElse(thatFunct.attempt(x))
    }
  }
}

object PartialFunct {
  def apply[A,B](f: PartialFunction[A,B]): PartialFunct[A,B] = {
    f match {
      case p: PartialFunct[A,B] => p
      case _ =>
        if (true) {
          new PartialFunctionWrapperUsingCatch[A,B](f)
        } else {
          new PartialFunctionWrapperUsingLift[A,B](f)
        }
    }
  }
}

private class PartialFunctionWrapperUsingCatch[-A, +B](f: PartialFunction[A,B]) extends PartialFunct[A, B] {
  def isDefinedAt(x: A) = f.isDefinedAt(x)

  def attempt(x: A) = {
    try {
      Some(f.apply(x))
    } catch {
      case _: MatchError => None
    }
  }
}

private class PartialFunctionWrapperUsingLift[-A, +B](f: PartialFunction[A,B]) extends PartialFunct[A, B] {
  override val lift = f.lift

  def isDefinedAt(x: A) = f.isDefinedAt(x)

  def attempt(x: A) = lift.apply(x)
}
