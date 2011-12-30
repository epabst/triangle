package com.github.triangle

import com.github.triangle.PortableField._
import actors.threadpool.AtomicInteger
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.Spec
import org.scalatest.matchers.MustMatchers
import collection.mutable

/** A Specification for all types of BaseField.
  * @author Eric Pabst (epabst@gmail.com)
  */
@RunWith(classOf[JUnitRunner])
abstract class BaseFieldContractSpec extends Spec with MustMatchers {
  def toBaseField[T](field: PortableField[T]): BaseField

  object StringIdentityField extends Field(identityField[String])
  object IntSetIdentityField extends Field(identityField[Set[Int]])

  val fieldWithGetterFromItem = GetterFromItem[String] {
    case StringIdentityField(Some(string)) && IntSetIdentityField(Some(set)) => Some(string + set.sum)
  }

  val fieldWithSetterUsingItems = new TransformerUsingSetter[Int] with NoGetter[Int] {
    def setter = throw new UnsupportedOperationException

    override def setterUsingItems: PartialFunction[(AnyRef, List[AnyRef]), Option[Int] => Unit] = {
      case (integer: AtomicInteger, IntSetIdentityField(Some(integers))) => value =>
        integer.set(value.getOrElse(0) + integers.sum)
    }
  }

  val baseFieldWithSetterUsingItems = toBaseField(default(100) + fieldWithSetterUsingItems)

  describe("copy") {
    class MyEntity(var myString: String, var number: Int)
    class OtherEntity(var name: String, var myBoolean: Boolean)

    it("must set defaults") {
      val stringField = toBaseField(Setter((e: MyEntity) => (v: String) => e.myString = v, noSetterForEmpty) + default("Hello"))

      val myEntity1 = new MyEntity("my1", 15)
      stringField.copy(PortableField.UseDefaults, myEntity1)
      myEntity1.myString must be ("Hello")
      myEntity1.number must be (15)
      //shouldn't fail
      stringField.copy(myEntity1, PortableField.UseDefaults)
    }

    it("must have the new value and the original 'from' available for the setter to use") {
      val integer = new AtomicInteger(30)
      baseFieldWithSetterUsingItems.copy(Set(1,0,3), integer)
      integer.get() must be (4)
    }

    it("must call the setter even if the getter isn't applicable") {
      val stringField = toBaseField(default("Hello") + mapField("greeting"))
      val map = mutable.Map[String,Any]("greeting" -> "Hola")
      stringField.copy(new Object, map)
      map.get("greeting") must be (None)
    }

    it("must copy from one to multiple") {
      val stringField = toBaseField(
        Getter[OtherEntity,String](e => e.name).withSetter(e => e.name = _, noSetterForEmpty) +
        Getter[MyEntity,String](e => e.myString).withSetter(e => e.myString = _, noSetterForEmpty)
      )

      val myEntity1 = new MyEntity("my1", 1)
      val otherEntity1 = new OtherEntity("other1", false)
      stringField.copy(myEntity1, otherEntity1)
      myEntity1.myString must be ("my1")
      myEntity1.number must be (1)
      otherEntity1.name must be ("my1")
      otherEntity1.myBoolean must be (false)

      val otherEntity2 = new OtherEntity("other2", true)
      val myEntity2 = new MyEntity("my2", 2)
      stringField.copy(otherEntity2, myEntity2)
      otherEntity2.name must be ("other2")
      otherEntity2.myBoolean must be (true)
      myEntity2.myString must be ("other2")
      myEntity2.number must be (2)

      stringField.copy(new Object, myEntity2) //does nothing

      stringField.copy(otherEntity2, new Object) //does nothing
    }
  }

  describe("copyFromItem") {
    it("must preserve semantics of getterFromItem on delegate") {
      val field = toBaseField[String](fieldWithGetterFromItem + mapField("string"))
      val map = mutable.Map.empty[String, Any]
      field.copyFromItem(List("hello", "ignored", Set(1,0,3)), map)
      map.get("string") must be (Some("hello4"))
    }

    it("must have the new value and the original items available for the setter to use") {
      val integer = new AtomicInteger(30)
      baseFieldWithSetterUsingItems.copyFromItem(List(PortableField.UseDefaults, Set(1,0,3)), integer)
      integer.get() must be (104)
    }
  }
}
