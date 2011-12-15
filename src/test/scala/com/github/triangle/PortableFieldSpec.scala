package com.github.triangle

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.matchers.MustMatchers
import org.scalatest.Spec
import PortableField._
import org.scalatest.mock.EasyMockSugar
import collection.{immutable, mutable}
import mutable.Buffer
import Converter._

/**
 * A behavior specification for {@link PortableField}.
 * @author Eric Pabst (epabst@gmail.com)
 * Date: 2/9/11
 * Time: 7:59 PM
 */

@RunWith(classOf[JUnitRunner])
class PortableFieldSpec extends Spec with MustMatchers with EasyMockSugar {
  val unitAsRef = Unit.asInstanceOf[AnyRef]

  object LengthField extends Getter[Int] {
    def getter = { case s: String => s.length }
  }

  describe("PortableField") {
    class MyEntity(var myString: String, var number: Int)
    class OtherEntity(var name: String, var myBoolean: Boolean)

    it("must be easily instantiable for an Entity") {
      Getter[MyEntity,String](e => e.myString).withSetter(e => e.myString = _, noSetterForEmpty)
      Getter[MyEntity,Int](e => e.number).withSetter(e => e.number = _, noSetterForEmpty)
      Getter[MyEntity,String](e => e.myString).withSetter(e => e.myString = _, noSetterForEmpty) +
        Getter[OtherEntity,String](e => e.name).withSetter(e => e.name = _, noSetterForEmpty)
      Getter[MyEntity,Int](e => e.number)
    }

    describe("copy") {
      it("must set defaults") {
        val stringField = Setter((e: MyEntity) => (v: String) => e.myString = v, noSetterForEmpty) + default("Hello")

        val myEntity1 = new MyEntity("my1", 15)
        stringField.copy(unitAsRef, myEntity1)
        myEntity1.myString must be ("Hello")
        myEntity1.number must be (15)
        //shouldn't fail
        stringField.copy(myEntity1, unitAsRef)
      }

      it("must happen if getter and setter are applicable") {
        val stringField = default("Hello") + mapField("greeting")
        val map = mutable.Map[String,Any]("greeting" -> "Hola")
        stringField.getter(Unit) must be (Some("Hello"))
        stringField.copy(unitAsRef, map)
        map.get("greeting") must be (Some("Hello"))
      }

      it("must call the setter even if the getter isn't applicable") {
        val stringField = default("Hello") + mapField("greeting")
        val map = mutable.Map[String,Any]("greeting" -> "Hola")
        stringField.getter.isDefinedAt(new Object) must be (false)
        stringField.copy(new Object, map)
        map.get("greeting") must be (None)
      }

      it("must copy from one to multiple") {
        val stringField =
          Getter[OtherEntity,String](e => e.name).withSetter(e => e.name = _, noSetterForEmpty) +
          Getter[MyEntity,String](e => e.myString).withSetter(e => e.myString = _, noSetterForEmpty)

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

    it("must extract values") {
      val LengthField(Some(length)) = "Hello"
      length must be (5)
    }

    it("must extract values from items") {
      val LengthField(Some(length)) = List(new Object, "Hello", new Object)
      length must be (5)
    }

    describe("default") {
      it("must only work on Unit") {
        val stringField = default("Hello")
        stringField.getter.isDefinedAt(List("bogus list")) must be (false)
        stringField.getter(Unit) must be (Some("Hello"))
      }
    }

    describe("adjustment") {
      it("must always happen when copying from Unit and it's the right kind of subject") {
        val adjust = adjustment[List[String]]("hello" +: _)
        adjust.transform(List("world"), unitAsRef).toList must equal(List("hello", "world"))
      }
    }

    describe("adjustmentInPlace") {
      it("must always happen when copying from Unit and it's the right kind of subject") {
        val adjust = adjustmentInPlace[collection.mutable.Buffer[String]] { _.append("world") }
        val buffer = collection.mutable.Buffer[String]("hello")
        adjust.copy(unitAsRef, buffer)
        buffer.toList must equal(List("hello", "world"))
      }
    }

    describe("mapField") {
      it("must remove a value from a mutable.Map") {
        val stringField = mapField[String]("greeting")
        val map = mutable.Map("greeting" -> "Hola")
        stringField.getter(mutable.Map[String,Any]()) must be (None)
        stringField.copy(mutable.Map[String,Any](), map)
        map.contains("greeting") must be (false)
      }

      it("must unwrap a Some when putting it into a mutable.Map") {
        val stringField = mapField[String]("greeting")
        val map = mutable.Map.empty[String,String]
        stringField.setValue(map, Some("Hello"))
        map.toMap must be (Map("greeting" -> "Hello"))
      }

      it("must not add to a mutable.Map if value is None") {
        val stringField = mapField[String]("greeting")
        val map = mutable.Map.empty[String,String]
        stringField.setValue(map, None)
        map.toMap must be (Map.empty[String,String])
      }

      it("must unwrap a Some when putting it into an immutable.Map") {
        val stringField = mapField[String]("greeting")
        val result = stringField.transformer[Map[String, String]](Map.empty)(Some("Hello"))
        result must be (Map("greeting" -> "Hello"))
      }

      it("must not add to an immutable.Map if value is None") {
        val stringField = mapField[String]("greeting")
        val result = stringField.transform(Map.empty[String,String], None)
        result must be (Map.empty[String,String])
      }
    }

    describe("copyFrom") {
      it("must return a working PortableValue if getter isDefinedAt") {
        val stringField = default("Hello") + mapField("greeting")
        stringField.getter(Unit) must be (Some("Hello"))
        val portableValue: PortableValue = stringField.copyFrom(unitAsRef)
        portableValue must not(be(null))

        val map = mutable.Map[String,Any]()
        portableValue.copyTo(map)
        map.get("greeting") must be (Some("Hello"))
        portableValue.get(stringField) must be (Some("Hello"))
      }

      it("must return a working PortableValue if getter !isDefinedAt") {
        val stringField = default("Hello") + mapField("greeting")
        val portableValue: PortableValue = stringField.copyFrom("string")
        portableValue must not(be(null))
        portableValue.get(stringField) must be (None)
        portableValue.get(default("Hi")) must be (None)

        val map = mutable.Map[String,Any]("greeting" -> "obsolete value")
        portableValue.copyTo(map)
        map.get("greeting") must be (None)
      }
    }

    describe("copyFromItem(from)") {
      it("must return a working PortableValue") {
        val stringField = mapField[String]("greeting")
        val intField = mapField[Int]("times")
        val map = Map[String, Any]("greeting" -> "hi", "times" -> 2)
        val portableValue: PortableValue = FieldList(stringField, intField).copyFromItem(List(new Object, map))
        portableValue.get(stringField) must be (Some("hi"))
        portableValue.get(intField) must be (Some(2))
      }

    }

    describe("copyFromItem(from, to)") {
      it("must use the first applicable field variation with the first applicable item") {
        val myEntity1 = new MyEntity("my1", 1)
        val otherEntity1 = new OtherEntity("other1", false)
        val stringField = mapField[String]("stringValue") +
          Getter[OtherEntity,String](e => e.name).withSetter(e => e.name = _, noSetterForEmpty) +
          Getter[MyEntity,String](e => e.myString).withSetter(e => e.myString = _, noSetterForEmpty)
        stringField.getterFromItem.isDefinedAt(List(myEntity1, otherEntity1)) must be (true)
        stringField.getterFromItem.isDefinedAt(List(new Object)) must be (false)
        stringField.getterFromItem(List(myEntity1, otherEntity1)) must be (Some("other1"))
        stringField.getterFromItem(List(otherEntity1, myEntity1)) must be (Some("other1"))
        val mutableMap = mutable.Map.empty[String, Any]
        stringField.copyFromItem(List(myEntity1, otherEntity1), mutableMap)
        mutableMap("stringValue") must be ("other1")
      }
    }

    describe("getterFromItem instance method") {
      it("must get from the first applicable item with Some value") {
        val fieldWithDefault = default(12) + mapField[Int]("count")
        fieldWithDefault.getterFromItem(List(immutable.Map.empty[String,Any], unitAsRef)) must be (Some(12))
        fieldWithDefault.getterFromItem(List(immutable.Map.empty[String,Any])) must be (None)

        val fieldWithoutDefault = mapField[Int]("count")
        fieldWithoutDefault.getterFromItem(List(immutable.Map.empty[String,Any], unitAsRef)) must be (None)

        val fieldWithDeprecatedName = mapField[Int]("count") + mapField[Int]("size")
        fieldWithDeprecatedName.getterFromItem(List(immutable.Map[String,Any]("size" -> 4))) must be (Some(4))
      }
    }

    describe("getterFromItem factory method") {
      object StringIdentityField extends Field(identityField[String])
      object Tuple2IdentityField extends Field(identityField[(Int,Int)])

      val field = getterFromItem[String] {
        case StringIdentityField(Some(string)) && Tuple2IdentityField(Some((x,y))) => string+ x + y
      }

      it("must handle accessing more than one item at once") {
        field.getterFromItem(List(1 -> 2, "hello")) must be (Some("hello12"))
      }

      it("must gracefully handle a non-list") {
        field.getter.isDefinedAt("hello") must be (false)
      }
    }

    describe("setter") {
      it("must call clearer if no value") {
        val stringField = Setter({ (b: Buffer[String]) => (v: String) => b += v; Unit }, (b: Buffer[String]) => b.clear())
        val buffer = Buffer("hello")
        stringField.setValue(buffer, None)
        buffer must be ('empty)
      }
    }

    describe("formatted") {
      it("must parse and format values") {
        val field = formatted[Double](ValueFormat.currencyValueFormat, mapField[String]("amountString"))
        field(Map("amountString" -> "12.34")) must be (12.34)

        val map = mutable.Map[String,Any]()
        field.setValue(map, Some(16))
        map("amountString") must be ("16.00")
      }

      it("must have a working transformer") {
        val formattedField = formatted[Int](mapField[String]("countString"))
        //qualified to point out that it's immutable
        val result: Map[String,Int] = formattedField.transformer[immutable.Map[String,Int]](immutable.Map.empty)(4)
        result.get("countString") must be (Some("4"))
      }
    }

    describe("converted") {
      it("must convert values in both directions") {
        val field: PortableField[Double] = converted(currencyToEditString, stringToCurrency, mapField[String]("amountString"))
        field(Map("amountString" -> "12.34")) must be (12.34)

        val map = mutable.Map[String,Any]()
        field.setValue(map, Some(16))
        map("amountString") must be ("16.00")
      }

      it("must have a working transformer") {
        val field = converted(currencyToString, stringToCurrency, mapField[String]("amountString"))
        //qualified to point out that it's immutable
        val result: Map[String,Double] = field.transformer[immutable.Map[String,Double]](immutable.Map.empty)(4.0)
        result.get("amountString") must be (Some("$4.00"))
      }
    }

    describe("transformer") {
      it("must delegate to setter for mutable objects") {
        val stringField = mapField[String]("greeting")
        val map = mutable.Map[String,Any]()
        val result: mutable.Map[String,Any] = stringField.transformer[mutable.Map[String,Any]](map)(Some("hello"))
        result.get("greeting") must be (Some("hello"))
        result must be (map)
      }

      it("must support transforming immutable objects") {
        val stringField = mapField[String]("greeting")
        val intField = mapField[Int]("count")
        //qualified to point out that it's immutable
        val result: Map[String,Any] = stringField.transformer[immutable.Map[String,Any]](immutable.Map.empty)(Some("hello"))
        result.get("greeting") must be (Some("hello"))

        val result2: Map[String,Any] = intField.transformer[Map[String,Any]](result)(Some(10))
        result2.get("greeting") must be (Some("hello"))
        result2.get("count") must be (Some(10))
      }

      it("must use all transformers of a field") {
        val stringField = mapField[String]("greeting") +
                Transformer((map: Map[String,String]) => ignored => map + ("greeting" -> map("greeting").toUpperCase),
                  (map: Map[String,String]) => map)
        //qualified to point out that it's immutable
        val result: Map[String,String] = stringField.transformer[immutable.Map[String,String]](immutable.Map.empty)(Some("hello"))
        result.get("greeting") must be (Some("HELLO"))
      }
    }

    describe("transform") {
      it("must use an initial and some data") {
        val stringField = mapField[String]("greeting") +
                Transformer((map: Map[String,String]) => ignored => map + ("greeting" -> map("greeting").toUpperCase),
                  (map: Map[String,String]) => map)
        //qualified to point out that it's immutable
        val result = stringField.transform(initial = immutable.Map.empty[String,String], data = immutable.Map("greeting" -> "hello", "ignored" -> "foo"))
        result must be (immutable.Map[String,String]("greeting" -> "HELLO"))
      }

      it("must not transform if initial subject is not applicable") {
        val stringField = mapField[String]("greeting")
        val result = stringField.transform(initial = "inapplicable data", data = immutable.Map("greeting" -> "hello", "ignored" -> "foo"))
        result must be ("inapplicable data")
      }

      it("must transform even if data is not applicable") {
        val stringField = mapField[String]("greeting")
        val result = stringField.transform(initial = immutable.Map[String,String]("greeting" -> "obsolete value"),
          data = "inapplicable data")
        result must be (immutable.Map.empty[String,String])
      }
    }

    describe("transformWithItem") {
      it("must transform using the applicable item") {
        val countField = default(12) + mapField[Int]("count")
        val result = countField.transformWithItem(initial = immutable.Map.empty[String,Any],
                                                  dataItems = List(new Object, Unit))
        result must be (immutable.Map[String,Any]("count" -> 12))
      }
    }

    describe("+") {
      it("must use getterForItem on each Field added together") {
        val mockField = mock[PortableField[String]]
        val field = mapField[String]("foo") + mockField
        expecting {
          call(mockField.getterFromItem).andReturn({
            case List(Unit, "String") => Some("success")
          }).anyTimes
        }
        whenExecuting(mockField) {
          field.getterFromItem(List(unitAsRef, "String")) must be (Some("success"))
        }
      }
    }

    it("must support easily specifying a getter as a partial function") {
      val field = Getter[Int] { case subject: AnyRef => 3 }
      field("hello") must be (3)
    }

    it("must support easily specifying a setter as a partial function") {
      val field = Setter[Int] { case subject: Buffer[Int] => _.foreach(value => subject += value) }
      val buffer = Buffer[Int](1, 2)
      field.setValue(buffer, Some(3))
      buffer.toList must be (List(1, 2, 3))
    }

    it("must support easily specifying a transformer as a partial function") {
      val field = Transformer[Int] {
        case subject: List[Int] => value => value.map(_ +: subject).getOrElse(subject)
      }
      field.transformWithValue(List(2, 3), Some(1)) must be (List(1, 2, 3))
    }

    describe("transformer") {
      it("must support None as the value") {
        val field = Transformer[Int] { case subject: List[Int] => _.map(value => value +: subject).getOrElse(subject) }
        field.transform(List(2, 3), None) must be (List(2, 3))
      }
    }

    describe("truncate") {
      it("must remove package names from the result") {
        val field = default(2)
        object Foo
        field.truncate(Foo) must(not(include(Foo.getClass.getPackage.getName.take(8))))
      }
    }
  }

  describe("&&") {
    it("must extract both values") {
      object FirstLetter extends Getter[Char] {
        def getter = { case s: String => s.head }
      }
      val LengthField(Some(length)) && FirstLetter(Some(c)) = "Hello"
      length must be (5)
      c must be ('H')
    }
  }
}
