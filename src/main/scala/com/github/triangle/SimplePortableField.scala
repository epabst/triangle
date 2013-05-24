package com.github.triangle

/**
 * A PortableField with the getter and updater provided in the constructor.
 * This may be combined into PortableField if everything starts using it.
 * @author Eric Pabst (epabst@gmail.com)
 *         Date: 5/9/13
 *         Time: 7:21 AM
 * @param getter a PartialFunction for getting an optional value from the first AnyRef in the GetterInput that has Some value.
 *               If none of them has Some value, then it will return None if at least one of them applies.
 *               If none of them even apply, the PartialFunction won't match at all (i.e. isDefinedAt will be false).
 * @param _updater a PartialFunction for updating an AnyRef using an optional value and context.
 *                 {{{updater(UpdaterInput(foo, valueOpt, GetterInput(...))}}} should return an updated version of foo
 *                 (which could be the same instance if mutable).
 *                 Note: Implementations usually must specify the return type to compile properly.
 *                 The parameter's subject is what will be updated, whether immutable or mutable.
 *                 The return value is ignored if the subject is mutable (and presumably updated in place).
 */
class SimplePortableField[T](val getter: PartialFunction[GetterInput,Option[T]] = PortableField.emptyPartialFunction,
                             _updater: PartialFunction[UpdaterInput[AnyRef,T],AnyRef] = PortableField.emptyField.updater) extends PortableField[T] {
  def updater[S <: AnyRef] = _updater.asInstanceOf[PartialFunction[UpdaterInput[S,T],S]]
}

object SimplePortableField {
  /** Does nothing but help specify the type for conciseness. */
  def asPartialFunction[T,F](function: PartialFunction[F,T]): PartialFunction[F,T] = function

  /** Does nothing but help specify the type for conciseness. */
  def asGetterFunction[T](getter: PartialFunction[GetterInput,Option[T]]): PartialFunction[GetterInput,Option[T]] = getter

  /** Does nothing but help specify the type for conciseness. */
  def asUpdaterFunction[T](updater: PartialFunction[UpdaterInput[AnyRef,T],AnyRef]): PartialFunction[UpdaterInput[AnyRef,T],AnyRef] =
    updater
}