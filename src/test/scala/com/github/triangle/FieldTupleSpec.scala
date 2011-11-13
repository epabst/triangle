package com.github.triangle

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.matchers.MustMatchers
import org.scalatest.Spec
import PortableField._
import scala.collection.mutable

/**
 * A behavior specification for [[com.github.triangle.FieldTuple]].
 * @author Eric Pabst (epabst@gmail.com)
 * Date: 4/20/11
 * Time: 1:39 PM
 */

@RunWith(classOf[JUnitRunner])
class FieldTupleSpec extends Spec with MustMatchers {
  val intField = default[Int](10) + mapField("int")
  val stringField = default[String]("Hello") + mapField("string")
  val doubleField = default[Double](11.0) + mapField("double")

  describe("valuesTuple") {
    it("must extract the field values") {
      val fieldTuple = FieldTuple3(intField, stringField, doubleField)
      //use it to match a single AnyRef
      fieldTuple.valuesTuple(Unit.asInstanceOf[AnyRef]) match {
        case (myInt, myString, myDouble) => {
          myInt must be (Some(10))
          myString must be (Some("Hello"))
          myDouble must be (Some(11.0))
        }
      }
      //use it to match a List of AnyRefs
      fieldTuple.valuesTupleFromItem(List(Unit)) match {
        case (myInt, myString, myDouble) => {
          myInt must be (Some(10))
          myString must be (Some("Hello"))
          myDouble must be (Some(11.0))
        }
      }
      //use it external from the tuple itself
      val (integer, string, double) = fieldTuple.valuesTuple(Unit.asInstanceOf[AnyRef])
      integer must be (Some(10))
      string must be (Some("Hello"))
      double must be (Some(11.0))
    }

    it("must work for tuple size of 2") {
      val tuple = FieldTuple2(intField, stringField)
      tuple.valuesTuple(Unit.asInstanceOf[AnyRef]) match {
        case (Some(10), Some("Hello")) => "ok"; case _ => fail()
      }
    }

    it("must work for tuple size of 3") {
      val tuple = FieldTuple3(intField, stringField, doubleField)
      tuple.valuesTuple(Unit.asInstanceOf[AnyRef]) match {
        case (Some(10), Some("Hello"), Some(11.0)) => "ok"; case _ => fail()
      }
    }

    it("must work for tuple size of 4") {
      val tuple = FieldTuple4(intField, stringField, doubleField, stringField)
      tuple.valuesTuple(Unit.asInstanceOf[AnyRef]) match {
        case (Some(10), Some("Hello"), Some(11.0), Some("Hello")) => "ok"; case _ => fail()
      }
    }

    it("must work for tuple size of 5") {
      val tuple = FieldTuple5(intField, stringField, doubleField, stringField, intField)
      tuple.valuesTuple(Unit.asInstanceOf[AnyRef]) match {
        case (Some(10), Some("Hello"), Some(11.0), Some("Hello"), Some(10)) => "ok"; case _ => fail()
      }
    }

    it("must work for tuple size of 6") {
      val tuple = FieldTuple6(intField, stringField, doubleField, stringField, intField, doubleField)
      tuple.valuesTuple(Unit.asInstanceOf[AnyRef]) match {
        case (Some(10), Some("Hello"), Some(11.0), Some("Hello"), Some(10), Some(11.0)) => "ok"; case _ => fail()
      }
    }

    it("must work for tuple size of 7") {
      val tuple = FieldTuple7(intField, stringField, doubleField, stringField, intField, doubleField, intField)
      tuple.valuesTuple(Unit.asInstanceOf[AnyRef]) match {
        case (Some(10), Some("Hello"), Some(11.0), Some("Hello"), Some(10), Some(11.0), Some(10)) => "ok"; case _ => fail()
      }
    }
  }

  describe("setter") {
    case class IntStringDouble(int: Int, string: String, double: Double)
    val setter = FieldTuple3(intField, stringField, doubleField).setter[IntStringDouble](v => (v.int, v.string, v.double))

    it("should use each inner field's setter") {
      val map = mutable.Map.empty[String,Any]
      setter.setValue(map, Some(IntStringDouble(5, "Joe", 0.1)))
      map.toMap must be (Map("int" -> 5, "string" -> "Joe", "double" -> 0.1))
    }

    it("should use each inner field's setter when no value is provided") {
      val map = mutable.Map[String,Any]("int" -> 5, "string" -> "Joe", "double" -> 0.1)
      setter.setValue(map, None)
      map.toMap must be (Map.empty[String,Any])
    }

    it("should support deepCollect") {
      setter.deepCollect {
        case f if f == intField => f
        case f if f == doubleField => f
      } must be (List(intField, doubleField))
    }
  }
}