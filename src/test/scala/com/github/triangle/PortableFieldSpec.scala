package com.github.triangle

import converter.ValueFormat
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import PortableField._
import org.scalatest.mock.EasyMockSugar
import collection.mutable
import converter.Converter._
import scala.collection.immutable

/** A behavior specification for [[com.github.triangle.PortableField]].
  * @author Eric Pabst (epabst@gmail.com)
  */

@RunWith(classOf[JUnitRunner])
class PortableFieldSpec extends BaseFieldContractSpec with EasyMockSugar {
  object LengthField extends SingleGetter[Int] {
    val singleGetter: PartialFunction[AnyRef,Option[Int]] = { case s: String => s.length }
  }

  //required by contract spec
  def toBaseField[T](field: PortableField[T]) = field

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
      it("must happen if getter and setter are applicable") {
        val stringField = default("Hello") + mapField("greeting")
        val map = mutable.Map[String,Any]("greeting" -> "Hola")
        stringField.getValue(PortableField.UseDefaults) must be (Some("Hello"))
        stringField.copy(PortableField.UseDefaults, map)
        map.get("greeting") must be (Some("Hello"))
      }

      it("must not evaluate getter if updater is not applicable") {
        val stringField = Getter[AnyRef,String](_ => sys.error("should not be used"))
        stringField.copy(PortableField.UseDefaults, new Object)
      }
    }

    it("must extract values") {
      val LengthField(Some(length)) = "Hello"
      length must be (5)
    }

    it("must extract values from items") {
      val LengthField(Some(length)) = GetterInput(new Object, "Hello", new Object)
      length must be (5)
    }

    describe("default") {
      it("must only work on PortableField.UseDefaults") {
        val stringField = default("Hello")
        stringField.getter.isDefinedAt(GetterInput.single(List("bogus list"))) must be (false)
        stringField.getter(GetterInput.single(PortableField.UseDefaults)) must be (Some("Hello"))
      }
    }

    describe("adjustment") {
      it("must always happen when copying from Unit and it's the right kind of subject") {
        val adjust = adjustment[List[String]]("hello" +: _)
        adjust.copyAndUpdate(PortableField.UseDefaults, List("world")).toList must equal(List("hello", "world"))
      }
    }

    describe("adjustmentInPlace") {
      it("must always happen when copying from Unit and it's the right kind of subject") {
        val adjust = adjustmentInPlace[mutable.Buffer[String]] { _.append("world") }
        val buffer = mutable.Buffer[String]("hello")
        adjust.copy(PortableField.UseDefaults, buffer)
        buffer.toList must equal(List("hello", "world"))
      }
    }

    describe("mapField") {
      it("must remove a value from a mutable.Map") {
        val stringField = mapField[String]("greeting")
        val map = mutable.Map("greeting" -> "Hola")
        stringField.getValue(mutable.Map[String,Any]()) must be (None)
        stringField.copy(mutable.Map[String,Any](), map)
        map.contains("greeting") must be (false)
      }

      it("must unwrap a Some when putting it into a mutable.Map") {
        val stringField = mapField[String]("greeting")
        val map = mutable.Map.empty[String,String]
        stringField.updateWithValue(map, Some("Hello"))
        map.toMap must be (Map("greeting" -> "Hello"))
      }

      it("must not add to a mutable.Map if value is None") {
        val stringField = mapField[String]("greeting")
        val map = mutable.Map.empty[String,String]
        stringField.updateWithValue(map, None)
        map.toMap must be (Map.empty[String,String])
      }

      it("must unwrap a Some when putting it into a Map") {
        val stringField = mapField[String]("greeting")
        val result = stringField.updateWithValue(Map.empty[String,String], Some("Hello"))
        result must be (Map("greeting" -> "Hello"))
      }

      it("must not add to a Map if value is None") {
        val stringField = mapField[String]("greeting")
        val result = stringField.copyAndUpdate(None, Map.empty[String,String])
        result must be (Map.empty[String,String])
      }
    }

    describe("copyFrom") {
      it("must support copying a None value") {
        val stringField = mapField("greeting") + optionMapField("reply")
        val portableValue: PortableValue = stringField.copyFrom(Map.empty[String,Any])
        portableValue must not(be(null))
        portableValue.get(stringField) must be (None)

        val map = mutable.Map[String,Option[Any]]()
        portableValue.update(map)
        map.apply("reply") must be (None)
      }

      it("must return a working PortableValue if getter isDefinedAt") {
        val stringField = default("Hello") + mapField("greeting")
        stringField.getValue(PortableField.UseDefaults) must be (Some("Hello"))
        val portableValue: PortableValue = stringField.copyFrom(PortableField.UseDefaults)
        portableValue must not(be(null))

        val map = mutable.Map[String,Any]()
        portableValue.update(map)
        map.get("greeting") must be (Some("Hello"))
        portableValue.get(stringField) must be (Some("Hello"))
      }

      it("must return a no-op PortableValue if getter !isDefinedAt") {
        val stringField = default("Hello") + mapField("greeting")
        val portableValue: PortableValue = stringField.copyFrom("string")
        portableValue must not(be(null))
        portableValue.get(stringField) must be (None)
        portableValue.get(default("Hi")) must be (None)

        val map = mutable.Map[String,Any]("greeting" -> "obsolete value")
        portableValue.update(map)
        map.get("greeting") must be (Some("obsolete value"))
      }
    }

    describe("copyFrom(GetterInput)") {
      it("must return a working PortableValue") {
        val stringField = mapField[String]("greeting")
        val intField = mapField[Int]("times")
        val map = Map[String, Any]("greeting" -> "hi", "times" -> 2)
        val portableValue: PortableValue = FieldList(stringField, intField).copyFrom(GetterInput(new Object, map))
        portableValue.get(stringField) must be (Some("hi"))
        portableValue.get(intField) must be (Some(2))
      }
    }

    describe("copy(GetterInput, to)") {
      it("must use the first applicable field variation with the first applicable item") {
        val myEntity1 = new MyEntity("my1", 1)
        val otherEntity1 = new OtherEntity("other1", false)
        val stringField = mapField[String]("stringValue") +
          Getter[OtherEntity,String](e => e.name).withSetter(e => e.name = _, noSetterForEmpty) +
          Getter[MyEntity,String](e => e.myString).withSetter(e => e.myString = _, noSetterForEmpty)
        stringField.getter.isDefinedAt(GetterInput(myEntity1, otherEntity1)) must be (true)
        stringField.getter.isDefinedAt(GetterInput.single(new Object)) must be (false)
        stringField.getter(GetterInput(myEntity1, otherEntity1)) must be (Some("other1"))
        stringField.getter(GetterInput(otherEntity1, myEntity1)) must be (Some("other1"))
        val mutableMap = mutable.Map.empty[String, Any]
        stringField.copy(GetterInput(myEntity1, otherEntity1), mutableMap)
        mutableMap("stringValue") must be ("other1")
      }
    }

    describe("getter") {
      it("must get from the first applicable item with Some value") {
        val fieldWithDefault = default(12) + mapField[Int]("count")
        fieldWithDefault.getter(GetterInput(Map.empty[String,Any], PortableField.UseDefaults)) must be (Some(12))
        fieldWithDefault.getter(GetterInput.single(Map.empty[String,Any])) must be (None)

        val fieldWithoutDefault = mapField[Int]("count")
        fieldWithoutDefault.getter(GetterInput(Map.empty[String,Any], PortableField.UseDefaults)) must be (None)

        val fieldWithDeprecatedName = mapField[Int]("count") + mapField[Int]("size")
        fieldWithDeprecatedName.getter(GetterInput.single(Map[String,Any]("size" -> 4))) must be (Some(4))
      }
    }

    describe("Getter") {
      object StringIdentityField extends Field(identityField[String])
      object Tuple2IdentityField extends Field(identityField[(Int,Int)])

      val field = Getter[String] {
        case StringIdentityField(Some(string)) && Tuple2IdentityField(Some((x,y))) => string+ x + y
      }

      it("must handle accessing more than one item at once") {
        field.getter(GetterInput(1 -> 2, "hello")) must be (Some("hello12"))
      }

      it("must gracefully handle a non-list") {
        field.getter.isDefinedAt(GetterInput.single("hello")) must be (false)
      }
    }

    describe("setter") {
      it("must call clearer if no value") {
        val stringField = Setter({ (b: mutable.Buffer[String]) => (v: String) => b += v; Unit }, (b: mutable.Buffer[String]) => b.clear())
        val buffer = mutable.Buffer("hello")
        stringField.updateWithValue(buffer, None)
        buffer must be ('empty)
      }
    }

    describe("formatted") {
      it("must parse and format values") {
        val field = formatted[Double](ValueFormat.currencyValueFormat, mapField[String]("amountString"))
        field.getValue(Map("amountString" -> "12.34")) must be (Some(12.34))

        val map = mutable.Map[String,Any]()
        field.updateWithValue(map, Some(16))
        map("amountString") must be ("16.00")
      }

      it("must have a working updater") {
        val formattedField = formatted[Int](mapField[String]("countString"))
        //qualified to point out that it's immutable
        val result: Map[String,Int] = formattedField.updater(UpdaterInput(immutable.Map.empty[String,Int], 4))
        result.get("countString") must be (Some("4"))
      }
    }

    describe("converted") {
      it("must convert values in both directions") {
        val field: PortableField[Double] = converted(currencyToEditString, mapField[String]("amountString"), stringToCurrency)
        field.getValue(Map("amountString" -> "12.34")) must be (Some(12.34))

        val map = mutable.Map[String,Any]()
        field.updateWithValue(map, Some(16))
        map("amountString") must be ("16.00")
      }

      it("must have a working updater") {
        val field = converted(currencyToString, mapField[String]("amountString"), stringToCurrency)
        //qualified to point out that it's immutable
        val result: Map[String,Double] = field.updater(UpdaterInput(immutable.Map.empty[String,Double], -4.0))
        result.get("amountString") must be (Some("-4.00"))
      }
    }

    describe("updater") {
      it("must delegate to setter for mutable objects") {
        val stringField = mapField[String]("greeting")
        val map = mutable.Map[String,Any]()
        val result: mutable.Map[String,Any] = stringField.updater(UpdaterInput(map, Some("hello")))
        result.get("greeting") must be (Some("hello"))
        result must be (map)
      }

      it("must support updating immutable objects") {
        val stringField = mapField[String]("greeting")
        val intField = mapField[Int]("count")
        //qualified to point out that it's immutable
        val result: Map[String,Any] = stringField.updater(UpdaterInput(immutable.Map.empty[String,Any], Some("hello")))
        result.get("greeting") must be (Some("hello"))

        val result2: Map[String,Any] = intField.updater(UpdaterInput(result, Some(10)))
        result2.get("greeting") must be (Some("hello"))
        result2.get("count") must be (Some(10))
      }

      it("must use all updaters of a field") {
        val stringField = mapField[String]("greeting") +
          Updater((map: Map[String,String]) => ignored => map + ("greeting" -> map("greeting").toUpperCase),
            (map: Map[String,String]) => map)
        //qualified to point out that it's immutable
        val result: Map[String,String] = stringField.updateWithValue(Map.empty[String,String], Some("hello"))
        result.get("greeting") must be (Some("HELLO"))
      }
    }

    describe("copyAndUpdate") {
      it("must use an initial and some data") {
        val stringField = Updater[Map[String,String],String](
          (map: Map[String,String]) => (string: String) => map.updated("reply", string.toUpperCase), (_: Map[String,String]) => error("unexpected")) +
          mapField[String]("greeting")

        val result = stringField.copyAndUpdate[Map[String,String]](Map("greeting" -> "hello", "ignored" -> "foo"), initial = Map("name" -> "George"))
        result must be (Map("greeting" -> "hello", "reply" -> "HELLO", "name" -> "George"))
      }

      it("must not update if initial subject is not applicable") {
        val stringField = mapField[String]("greeting")
        val result = stringField.copyAndUpdate(Map("greeting" -> "hello", "ignored" -> "foo"), initial = "inapplicable data")
        result must be ("inapplicable data")
      }

      it("must not update if data is not applicable") {
        val stringField = mapField[String]("greeting")
        val initial = Map[String, String]("greeting" -> "obsolete value")
        val result = stringField.copyAndUpdate("inapplicable data", initial)
        result must be (initial)
      }

      it("must update if applicable even if value is None") {
        val stringField = optionMapField[String]("greeting")
        val data = Map[String, Option[String]]("greeting" -> None)
        val result = stringField.copyAndUpdate(data, Map.empty[String,Option[Any]])
        result must be (Map("greeting" -> None))
      }
    }

    describe("copyAndUpdate(GetterInput, ...)") {
      it("must update using the applicable item") {
        val countField = default(12) + mapField[Int]("count")
        val result = countField.copyAndUpdate(GetterInput(new Object, PortableField.UseDefaults),
          initial = Map.empty[String,Any])
        result must be (Map[String,Any]("count" -> 12))
      }
    }

    describe("+") {
      it("must use getterForItem on each Field added together") {
        val mockField = mock[PortableField[String]]
        val field = mapField[String]("foo") + mockField
        expecting {
          call(mockField.getterVal).andReturn({
            case GetterInput(List(PortableField.UseDefaults, "String")) => Some("success")
          }).anyTimes
        }
        whenExecuting(mockField) {
          field.getterVal(GetterInput(PortableField.UseDefaults, "String")) must be (Some("success"))
        }
      }
    }

    it("must support easily specifying a getter as a partial function") {
      val field = Getter.single[Int] { case subject: AnyRef => 3 }
      field.getValue("hello") must be (Some(3))
    }

    it("must support easily specifying a setter as a partial function") {
      val field = Setter[Int] {
        case UpdaterInput(subject: mutable.Buffer[Int], valueOpt, _) => valueOpt.foreach(value => subject += value)
      }
      val buffer = mutable.Buffer[Int](1, 2)
      field.updateWithValue(buffer, Some(3))
      buffer.toList must be (List(1, 2, 3))
    }

    it("must support easily specifying a updater as a partial function") {
      val field = Updater[Int] {
        case UpdaterInput(subject: List[Int], valueOpt, _) => valueOpt.map(_ +: subject).getOrElse(subject)
      }
      field.updateWithValue(List(2, 3), Some(1)) must be (List(1, 2, 3))
    }

    describe("Updater") {
      it("must support None as the value") {
        val field = Updater[Int] {
          case UpdaterInput(subject: List[Int], valueOpt, _) => valueOpt.map(_ +: subject).getOrElse(subject)
        }
        field.copyAndUpdate(None, List(2, 3)) must be (List(2, 3))
      }
    }

    describe("truncate") {
      it("must remove package names from the result") {
        object Foo
        PortableField.truncate(Foo) must(not(include(Foo.getClass.getPackage.getName.take(8))))
      }
    }
  }

  describe("&&") {
    it("must extract both values") {
      object FirstLetter extends SingleGetter[Char] {
        val singleGetter: PartialFunction[AnyRef,Option[Char]] = { case s: String => s.head }
      }
      val LengthField(Some(length)) && FirstLetter(Some(c)) = "Hello"
      length must be (5)
      c must be ('H')
    }
  }
}
