package com.github.triangle

import com.github.triangle.PortableField._
import actors.threadpool.AtomicInteger
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.Spec
import org.scalatest.matchers.MustMatchers

/**
 * A Specification for all types of BaseField.
 * @author Eric Pabst (epabst@gmail.com)
 */
@RunWith(classOf[JUnitRunner])
abstract class BaseFieldContractSpec extends Spec with MustMatchers {
  def toBaseField[T](field: PortableField[T]): BaseField

  object IntSetIdentityField extends Field(identityField[Set[Int]])

  val baseFieldWithSetterUsingItems = toBaseField(default(100) + new TransformerUsingSetter[Int] with NoGetter[Int] {
    def setter = throw new UnsupportedOperationException

    override def setterUsingItems: PartialFunction[(AnyRef, List[AnyRef]), Option[Int] => Unit] = {
      case (integer: AtomicInteger, IntSetIdentityField(Some(integers))) => value =>
        integer.set(value.getOrElse(0) + integers.sum)
    }
  })

  describe("copy") {
    it("must have the new value and the original 'from' available for the setter to use") {
      val integer = new AtomicInteger(30)
      baseFieldWithSetterUsingItems.copy(Set(1,0,3), integer)
      integer.get() must be (4)
    }
  }

  describe("copyFromItem") {
    it("must have the new value and the original items available for the setter to use") {
      val integer = new AtomicInteger(30)
      baseFieldWithSetterUsingItems.copyFromItem(List(PortableField.UseDefaults, Set(1,0,3)), integer)
      integer.get() must be (104)
    }
  }
}
