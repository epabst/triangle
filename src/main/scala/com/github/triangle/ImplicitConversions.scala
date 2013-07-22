package com.github.triangle

/**
 * todo A ... 
 * @author Eric Pabst (epabst@gmail.com)
 *         Date: 7/22/13
 *         Time: 7:23 AM
 */
object ImplicitConversions {
  /** This makes getters able to be written more simply by not having to explicitly wrap the result in a "Some". */
  implicit def toSome[T](value: T): Option[T] = Some(value)
}
