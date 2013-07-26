package com.github.triangle

/**
 * Any PortableField that contains multiple other PortableFields should extend this.
 * @see Field
 * @author Eric Pabst (epabst@gmail.com)
 *         Date: 7/26/13
 *         Time: 7:07 AM
 */
trait FieldWithDelegates[T] extends PortableField[T] {
  protected def delegates: Seq[BaseField]

  override def deepCollect[R](f: PartialFunction[BaseField, R]) = {
    val directCollectOpt = f.lift(this).map(List(_))
    directCollectOpt.getOrElse {
      delegates.flatMap(_.deepCollect(f))
    }
  }
}
