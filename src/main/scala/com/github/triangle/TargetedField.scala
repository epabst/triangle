package com.github.triangle

trait FieldWithSubjectManifest { self: PortableField[_] =>
  def subjectManifest: ClassManifest[_]
}

/** A PortableField that has a Subject that it applies to. */
trait TargetedField[S <: AnyRef,T] extends PortableField[T] with FieldWithSubjectManifest { self =>
  def subjectManifest: ClassManifest[S]

  def withSetter(body: S => Option[T] => Unit): TargetedField[S,T] =
    new Field[T](this + Setter(body)(subjectManifest)) with TargetedField[S,T] {
      def subjectManifest = self.subjectManifest
    }

  def withSetter(body: S => T => Unit, clearer: S => Unit): TargetedField[S,T] =
    new Field[T](this + Setter(body, clearer)(subjectManifest)) with TargetedField[S,T] {
      def subjectManifest = self.subjectManifest
    }

  def withUpdater(body: S => Option[T] => S): TargetedField[S,T] =
    new Field[T](this + Updater(body)(subjectManifest)) with TargetedField[S,T] {
      def subjectManifest = self.subjectManifest
    }

  def withUpdater(body: S => T => S, clearer: S => S): TargetedField[S,T] =
    new Field[T](this + Updater(body, clearer)(subjectManifest)) with TargetedField[S,T] {
      def subjectManifest = self.subjectManifest
    }
}
