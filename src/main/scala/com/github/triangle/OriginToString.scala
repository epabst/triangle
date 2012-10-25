package com.github.triangle

/**
 * A trait for making the toString include the place where it was created.
 * @author Eric Pabst (epabst@gmail.com)
 * Date: 10/24/12
 * Time: 10:50 PM
 */
trait OriginToString {
  private val packageNamesToExcludeForOriginToStringVal = packageNamesToExcludeForOriginToString
  protected def packageNamesToExcludeForOriginToString: Seq[String]

  private val origin: StackTraceElement = {
    val stackTrace = new Throwable().getStackTrace
    stackTrace.dropWhile(e => packageNamesToExcludeForOriginToStringVal.exists(e.getClassName.startsWith(_))).headOption.getOrElse(stackTrace.last)
  }

  override def toString = super.toString + " [from " + origin + "]"

  protected def withToString[A,B](string: String)(f: PartialFunction[A,B]): PartialFunction[A,B] = new PartialFunction[A,B] {
    def isDefinedAt(x: A) = f.isDefinedAt(x)

    def apply(x: A) = f.apply(x)

    override def toString() = string
  }
}
