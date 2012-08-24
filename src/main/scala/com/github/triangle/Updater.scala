package com.github.triangle

/** A PortableField with no updater. */
trait NoUpdater[T] extends PortableField[T] {
  def updater[S <: AnyRef] = PortableField.emptyPartialFunction
}

/** A PortableField that has an updater but no getter. */
trait Updater[T] extends NoGetter[T]

object Updater {
  /** Factory method that takes the body of the updater as a parameter. */
  def apply[T](body: PartialFunction[UpdaterInput[AnyRef,T],AnyRef]): Updater[T] = new Updater[T] {
    def updater[S <: AnyRef]: PartialFunction[UpdaterInput[S, T], S] = {
      case input if body.isDefinedAt(input.asInstanceOf[UpdaterInput[AnyRef,T]]) =>
        body(input.asInstanceOf[UpdaterInput[AnyRef,T]]).asInstanceOf[S]
    }
  }
}
