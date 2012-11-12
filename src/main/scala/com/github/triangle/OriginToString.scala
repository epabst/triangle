package com.github.triangle

/**
 * A trait for making the toString include the place where it was created.
 * @author Eric Pabst (epabst@gmail.com)
 * Date: 10/24/12
 * Time: 10:50 PM
 */
trait OriginToString {
  protected def packageNamesToExcludeForOriginToString: Seq[String]

  private val origin: StackTraceElement = {
    val stackTrace = new Throwable().getStackTrace
    val notableElements = stackTrace.dropWhile { e =>
      packageNamesToExcludeForOriginToString.exists { packageName =>
        e.getClassName.startsWith(packageName)
      }
    }
    notableElements.headOption.getOrElse(stackTrace.last)
  }

  private lazy val toStringVal = super.toString + " [from " + origin + "]"
  override def toString = toStringVal

  protected def withToString[A,B](string: String)(f: PartialFunction[A,B]): PartialFunction[A,B] = new PartialFunction[A,B] {
    def isDefinedAt(x: A) = f.isDefinedAt(x)

    def apply(x: A) = f.apply(x)

    override def toString() = string
  }
}
