package com.github.triangle

trait SubjectField { self: PortableField[_] =>
  def subjectManifest: ClassManifest[_]
}

/** A PortableField that has a Subject that it applies to. */
trait FieldWithSubject[S <: AnyRef,T] extends PortableField[T] with SubjectField { self =>
  def subjectManifest: ClassManifest[S]

  def withSetter(body: S => Option[T] => Unit): FieldWithSubject[S,T] =
    new Field[T](this + Setter(body)(subjectManifest)) with FieldWithSubject[S,T] {
      def subjectManifest = self.subjectManifest
    }

  def withSetter(body: S => T => Unit, clearer: S => Unit): FieldWithSubject[S,T] =
    new Field[T](this + Setter(body, clearer)(subjectManifest)) with FieldWithSubject[S,T] {
      def subjectManifest = self.subjectManifest
    }

  def withUpdater(body: S => Option[T] => S): FieldWithSubject[S,T] =
    new Field[T](this + SubjectUpdater(body)(subjectManifest)) with FieldWithSubject[S,T] {
      def subjectManifest = self.subjectManifest
    }

  def withUpdater(body: S => T => S, clearer: S => S): FieldWithSubject[S,T] =
    new Field[T](this + SubjectUpdater(body, clearer)(subjectManifest)) with FieldWithSubject[S,T] {
      def subjectManifest = self.subjectManifest
    }
}
