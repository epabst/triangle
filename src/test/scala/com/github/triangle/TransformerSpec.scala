package com.github.triangle

import com.github.triangle.PortableField._
import org.scalatest.Spec
import org.scalatest.junit.JUnitRunner
import org.junit.runner.RunWith
import org.scalatest.matchers.MustMatchers

/** A Specification for [[com.github.triangle.Transformer]].
  * @author Eric Pabst (epabst@gmail.com)
  */
@RunWith(classOf[JUnitRunner])
class TransformerSpec extends Spec with MustMatchers {
  case class MyEntity(name: String = "(none)")

  it("must provide a convenient clearer") {
    Transformer[String,String]((string: String) => (v: String) => string + v, noTransformerForEmpty[String])
  }

  describe("TransformerUsingItems") {
    val stringField: PortableField[String] =
      TransformerUsingItems((e: MyEntity, input) => v => e.copy(name = v.getOrElse("(none)") + input.items.mkString("-", "-", "")))

    it("must allow using the items while setting") {
      val entity = stringField.transformWithValue(new MyEntity(), Some("James"), List("Bond", "007"))
      entity.name must be ("James-Bond-007")
    }

    it("must be constructable with a partial function") {
      val field: PortableField[String] = TransformerUsingItems[String] {
        case (e: MyEntity, input) if input.items.contains("Bond") => v => e.copy(name = v.getOrElse("(none)") + input.items.mkString("-", "-", ""))
      }
      val entity = field.transformWithValue(new MyEntity(), Some("James"), List("Dean"))
      entity.name must be ("(none)")
    }
  }
}
