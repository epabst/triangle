package com.github.triangle

/**
 * A trait for making the toString include the place where it was created.
 * @author Eric Pabst (epabst@gmail.com)
 * Date: 10/24/12
 * Time: 10:50 PM
 */
trait OriginToString {
  protected def packageNamesToExcludeForOriginToString: Seq[String]

  private val originStackTrace = new Throwable().getStackTrace

  private lazy val toStringVal = {
    val notableElements = originStackTrace.dropWhile { e =>
      packageNamesToExcludeForOriginToString.exists { packageName =>
        e.getClassName.startsWith(packageName)
      }
    }
    val origin: StackTraceElement = notableElements.headOption.getOrElse(originStackTrace.last)
    super.toString + " [from " + origin + "]"
  }
  override def toString = toStringVal

  protected def withToString[A,B](string: String)(f: PartialFunct[A,B]): PartialFunct[A,B] = new PartialFunct[A,B] {
    def isDefinedAt(x: A) = f.isDefinedAt(x)

    def attempt(x: A) = f.attempt(x)

    override def toString() = string
  }
}
