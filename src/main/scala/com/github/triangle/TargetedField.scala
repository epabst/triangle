package com.github.triangle

trait FieldWithSubjectManifest { self: PortableField[_] =>
  def subjectManifest: ClassManifest[_]
}

/** A PortableField that has a Subject that it applies to. */
trait TargetedField[S <: AnyRef,T] extends PortableField[T] with FieldWithSubjectManifest { self =>
  def subjectManifest: ClassManifest[S]

  def withSetter(body: S => Option[T] => Unit): PortableField[T] =
    this + Setter(body)(subjectManifest)

  def withSetter(body: S => T => Unit, clearer: S => Unit): PortableField[T] =
    this + Setter(body, clearer)(subjectManifest)

  def withUpdater(body: S => Option[T] => S): PortableField[T] =
    this + Updater(body)(subjectManifest)

  def withUpdater(body: S => T => S, clearer: S => S): PortableField[T] =
    this + Updater(body, clearer)(subjectManifest)
}
