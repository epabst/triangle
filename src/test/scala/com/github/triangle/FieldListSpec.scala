package com.github.triangle

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import PortableField._
import scala.collection._


/** A behavior specification for [[com.github.triangle.FieldList]].
  * @author Eric Pabst (epabst@gmail.com)
  */

@RunWith(classOf[JUnitRunner])
class FieldListSpec extends BaseFieldContractSpec {
  //required by contract spec
  def toBaseField[T](field: PortableField[T]) = FieldList(field)

  describe("copy") {
    it("must copy values that apply") {
      val countField = default[Int](10) + mapField("count")
      val priceField = mapField[Double]("price")
      val fields = FieldList(countField, priceField)
      val map = mutable.Map[String, Any]()

      //copy where only one field has an accessor
      fields.copy(PortableField.UseDefaults, map)
      map.contains("count") must be (true)
      countField.getValue(map) must be (Some(10))
      map.contains("price") must be (false)

      fields.copy(mutable.Map("price" -> 300.00), map)
      map.contains("price") must be (true)
      priceField.getValue(map) must be (Some(300.00))
      //must have been overwritten because the Map didn't have it
      countField.getValue(map) must be (None)
    }

    it("must copy values that apply from list of items") {
      //intentionally put default before mapField
      val countField = default[Int](10) + mapField[Int]("count")
      val priceField = mapField[Double]("price")
      val fields = FieldList(countField, priceField)
      val map = mutable.Map[String, Any]()

      val itemList = GetterInput(new Object, PortableField.UseDefaults, Map("count" -> 11, "price" -> 300.0))
      fields.copy(itemList, map)
      //should use the default since first in the item list
      map.get("count") must be (Some(10))
      map.get("price") must be (Some(300.00))
    }

    it("must copyAndUpdate using each Field") {
      val countField = mapField[Int]("count")
      val priceField = mapField[Double]("price")
      val fields = FieldList(countField, priceField)
      val result = fields.copyAndUpdate(immutable.Map[String,Any]("ignored" -> "bar", "price" -> 100.0, "count" -> 10),
        initial = immutable.Map.empty[String,Any])
      result must be (immutable.Map[String,Any]("count" -> 10, "price" -> 100.0))
    }

    it("must update using the first applicable item for each Field") {
      val countField = default(12) + mapField[Int]("count")
      val priceField = mapField[Double]("price")
      val fields = FieldList(countField, priceField)
      val result = fields.copyAndUpdate(GetterInput(PortableField.UseDefaults, immutable.Map[String, Any]("ignored" -> "bar", "price" -> 100.0)),
        initial = immutable.Map.empty[String,Any])
      result must be (immutable.Map[String,Any]("count" -> 12, "price" -> 100.0))
    }
  }

  describe("copyableTo") {
    it("must only return fields that can update the given subject") {
      val countField = mapField[Int]("count")
      val priceField = mapField[Double]("price")
      val adjustmentField = adjustment[StringBuffer](_.append(" and more"))
      val fields = FieldList(countField, adjustmentField, priceField)
      fields.copyableTo(Map.empty[String,Any]).toString must be (FieldList(countField, priceField).toString())
      fields.copyableTo(new StringBuffer).toString must be (FieldList(adjustmentField).toString())
    }

    it("must only return fields that can update the given subject with the given context") {
      val appendField = Setter[String] {
        case UpdaterInput(sb: StringBuffer, valueOpt, GetterInput(List(suffix: String))) =>
          sb.append(valueOpt.getOrElse("none")).append(suffix)
      }
      val priceField = mapField[Double]("price")
      val fields = FieldList(appendField, priceField)
      fields.copyableTo(new StringBuffer, GetterInput.single("suffix")).toString must be (FieldList(appendField).toString())
    }
  }
}
