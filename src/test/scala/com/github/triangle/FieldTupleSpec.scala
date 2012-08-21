package com.github.triangle

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.matchers.MustMatchers
import org.scalatest.Spec
import PortableField._
import scala.collection.mutable

/** A behavior specification for [[com.github.triangle.FieldTuple]].
  * @author Eric Pabst (epabst@gmail.com)
  */

@RunWith(classOf[JUnitRunner])
class FieldTupleSpec extends Spec with MustMatchers {
  val intField = default[Int](10) + mapField("int")
  val stringField = default[String]("Hello") + mapField("string")
  val doubleField = default[Double](11.0) + mapField("double")
  case class IntStringDouble(int: Int, string: String, double: Double)

  describe("valuesTuple") {
    it("must extract the field values") {
      val fieldTuple = FieldTuple(intField, stringField, doubleField)
      //use it to match a GetterInput
      fieldTuple.valuesTupleFromItem(GetterInput.single(PortableField.UseDefaults)) match {
        case (myInt, myString, myDouble) => {
          myInt must be (Some(10))
          myString must be (Some("Hello"))
          myDouble must be (Some(11.0))
        }
      }
      //use it external from the tuple itself
      val (integer, string, double) = fieldTuple.valuesTupleFromItem(GetterInput.single(PortableField.UseDefaults))
      integer must be (Some(10))
      string must be (Some("Hello"))
      double must be (Some(11.0))
    }

    it("must work for tuple size of 2") {
      val tuple = FieldTuple(intField, stringField)
      tuple.valuesTupleFromItem(GetterInput.single(PortableField.UseDefaults)) match {
        case (Some(10), Some("Hello")) => "ok"; case _ => fail()
      }
    }

    it("must work for tuple size of 3") {
      val tuple = FieldTuple(intField, stringField, doubleField)
      tuple.valuesTupleFromItem(GetterInput.single(PortableField.UseDefaults)) match {
        case (Some(10), Some("Hello"), Some(11.0)) => "ok"; case _ => fail()
      }
    }

    it("must work for tuple size of 4") {
      val tuple = FieldTuple(intField, stringField, doubleField, stringField)
      tuple.valuesTupleFromItem(GetterInput.single(PortableField.UseDefaults)) match {
        case (Some(10), Some("Hello"), Some(11.0), Some("Hello")) => "ok"; case _ => fail()
      }
    }

    it("must work for tuple size of 5") {
      val tuple = FieldTuple(intField, stringField, doubleField, stringField, intField)
      tuple.valuesTupleFromItem(GetterInput.single(PortableField.UseDefaults)) match {
        case (Some(10), Some("Hello"), Some(11.0), Some("Hello"), Some(10)) => "ok"; case _ => fail()
      }
    }

    it("must work for tuple size of 6") {
      val tuple = FieldTuple(intField, stringField, doubleField, stringField, intField, doubleField)
      tuple.valuesTupleFromItem(GetterInput.single(PortableField.UseDefaults)) match {
        case (Some(10), Some("Hello"), Some(11.0), Some("Hello"), Some(10), Some(11.0)) => "ok"; case _ => fail()
      }
    }

    it("must work for tuple size of 7") {
      val tuple = FieldTuple(intField, stringField, doubleField, stringField, intField, doubleField, intField)
      tuple.valuesTupleFromItem(GetterInput.single(PortableField.UseDefaults)) match {
        case (Some(10), Some("Hello"), Some(11.0), Some("Hello"), Some(10), Some(11.0), Some(10)) => "ok"; case _ => fail()
      }
    }
  }

  describe("transformWithValues") {
    it("must work for tuple size of 2") {
      val tuple = FieldTuple(intField, stringField)
      tuple.transformWithValues(Map.empty[String,Any], (Some(2), Some("hi"))) must
        be (Map("int" -> 2, "string" -> "hi"))
    }

    it("must work for tuple size of 3") {
      val tuple = FieldTuple(intField, stringField, doubleField)
      tuple.transformWithValues(Map.empty[String,Any], (Some(2), Some("hi"), Some(3.14))) must
        be (Map("int" -> 2, "string" -> "hi", "double" -> 3.14))
    }

    it("must work for tuple size of 4") {
      val tuple = FieldTuple(intField, stringField, doubleField, stringField)
      tuple.transformWithValues(Map.empty[String,Any], (Some(2), Some("hi"), Some(3.14), Some("hi2"))) must
        be (Map("int" -> 2, "string" -> "hi2", "double" -> 3.14))
    }

    it("must work for tuple size of 5") {
      val tuple = FieldTuple(intField, stringField, doubleField, stringField, intField)
      tuple.transformWithValues(Map.empty[String,Any], (Some(2), Some("hi"), Some(3.14), Some("hi2"), Some(3))) must
        be (Map("int" -> 3, "string" -> "hi2", "double" -> 3.14))
    }

    it("must work for tuple size of 6") {
      val tuple = FieldTuple(intField, stringField, doubleField, stringField, intField, doubleField)
      tuple.transformWithValues(Map.empty[String,Any], (Some(2), Some("hi"), Some(3.14), Some("hi2"), Some(3), Some(1.77))) must
        be (Map("int" -> 3, "string" -> "hi2", "double" -> 1.77))
    }

    it("must work for tuple size of 7") {
      val tuple = FieldTuple(intField, stringField, doubleField, stringField, intField, doubleField, intField)
      tuple.transformWithValues(Map.empty[String,Any], (Some(2), Some("hi"), Some(3.14), Some("hi2"), Some(3), Some(1.77), Some(11))) must
        be (Map("int" -> 11, "string" -> "hi2", "double" -> 1.77))
    }
  }

  describe("getter") {
    val getter = FieldTuple(intField, stringField, doubleField).getter[IntStringDouble] {
      case (Some(int), Some(string), Some(double)) => Some(IntStringDouble(int, string, double))
      case _ => None
    }

    it("should use each inner field's getter") {
      val map = Map("int" -> 5, "string" -> "Joe", "double" -> 0.1)
      getter.getValue(map) must be (Some(IntStringDouble(5, "Joe", 0.1)))
    }

    it("should support deepCollect") {
      getter.deepCollect {
        case f if f == intField => f
        case f if f == doubleField => f
      } must be (List(intField, doubleField))
    }

    describe("withTransformer") {
      val withTransformer = getter.withTransformer(v => FieldTuple.toTuple3OfSomes[Int,String,Double]((v.int, v.string, v.double)))

      it("must be supported") {
        val map = withTransformer.transformWithValue(Map.empty[String,Any], Some(IntStringDouble(5, "Joe", 0.1)))
        map must be (Map("int" -> 5, "string" -> "Joe", "double" -> 0.1))
      }

      describe("deepCollect") {
        it("must only return each match once") {
          withTransformer.deepCollect {
            case f if f == intField => f
          } must be (List(intField))
        }

        it("must match the field itself") {
          withTransformer.deepCollect {
            case f if f == withTransformer => f
          } must be (List(withTransformer))
        }

        it("must match the getter") {
          withTransformer.deepCollect {
            case f if f == getter => f
          } must be (List(getter))
        }
      }
    }
  }

  describe("transformer") {
    val transformer = FieldTuple(intField, stringField, doubleField).transformer[IntStringDouble](v => (v.int, v.string, v.double))

    it("should use each inner field's transformer") {
      val map = transformer.transformWithValue(Map.empty[String,Any], Some(IntStringDouble(5, "Joe", 0.1)))
      map must be (Map("int" -> 5, "string" -> "Joe", "double" -> 0.1))
    }

    it("should use each inner field's transformer when no value is provided") {
      val map = transformer.transformWithValue(Map[String,Any]("int" -> 5, "string" -> "Joe", "double" -> 0.1), None)
      map.toMap must be (Map.empty[String,Any])
    }

    it("should use each inner field's setter") {
      val map = mutable.Map.empty[String,Any]
      transformer.setValue(map, Some(IntStringDouble(5, "Joe", 0.1)))
      map.toMap must be (Map("int" -> 5, "string" -> "Joe", "double" -> 0.1))
    }

    it("should use each inner field's setter when no value is provided") {
      val map = mutable.Map[String,Any]("int" -> 5, "string" -> "Joe", "double" -> 0.1)
      transformer.setValue(map, None)
      map.toMap must be (Map.empty[String,Any])
    }

    it("should support deepCollect") {
      transformer.deepCollect {
        case f if f == intField => f
        case f if f == doubleField => f
      } must be (List(intField, doubleField))
    }
  }
}