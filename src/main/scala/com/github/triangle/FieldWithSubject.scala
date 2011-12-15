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

  def withTransformer(body: S => Option[T] => S): FieldWithSubject[S,T] = new Field[T](this + Transformer(body)(subjectManifest)) with FieldWithSubject[S,T] {
    def subjectManifest = self.subjectManifest
  }

  def withTransformer(body: S => T => S, clearer: S => S): FieldWithSubject[S,T] =
    new Field[T](this + Transformer(body, clearer)(subjectManifest)) with FieldWithSubject[S,T] {
      def subjectManifest = self.subjectManifest
    }

  object Typed {
    def unapply(subject: AnyRef): Option[S] = subject match {
      //subjectManifest.erasure.isInstance(subject)
      case typedSubject: S => Some(typedSubject)
      case _ => None
    }
  }
}
