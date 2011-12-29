package com.github.triangle

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.Spec
import org.scalatest.matchers.MustMatchers

/** A specification for [[com.github.triangle.Setter]].
  * @author Eric Pabst (epabst@gmail.com)
  */
@RunWith(classOf[JUnitRunner])
class SetterSpec extends Spec with MustMatchers {
  class MyEntity(var name: String = "(none)")

  describe("SetterUsingItems") {
    val stringField: PortableField[String] =
      SetterUsingItems((e: MyEntity, items) => v => e.name = v.getOrElse("(none)") + items.mkString("-", "-", ""))
    
    it("must allow using the items while setting") {
      val entity = new MyEntity()
      stringField.setValue(entity, Some("James"), List("Bond", "007"))
      entity.name must be ("James-Bond-007")
    }

    it("must be constructable with a partial function") {
      val field: PortableField[String] = SetterUsingItems[String] {
        case (e: MyEntity, items) if items.contains("Bond") => v => e.name = v.getOrElse("(none)") + items.mkString("-", "-", "")
      }
      val entity = new MyEntity()
      field.setValue(entity, Some("James"), List("Dean")) must be (false)
      entity.name must be ("(none)")
    }
  }
}
