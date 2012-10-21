package com.github.triangle

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.FunSpec
import org.scalatest.matchers.MustMatchers

/** A specification for [[com.github.triangle.Setter]].
  * @author Eric Pabst (epabst@gmail.com)
  */
@RunWith(classOf[JUnitRunner])
class SetterSpec extends FunSpec with MustMatchers {
  class MyEntity(var name: String = "(none)")

  val stringField: PortableField[String] = Setter[String] {
    case UpdaterInput(e: MyEntity, valueOpt, input) =>
      e.name = valueOpt.getOrElse("(none)") + input.items.mkString("-", "-", "")
  }

  it("must allow using the items while setting") {
    val entity = new MyEntity()
    stringField.updateWithValue(entity, Some("James"), GetterInput("Bond", "007"))
    entity.name must be ("James-Bond-007")
  }

  it("must be constructable with a partial function") {
    val field: PortableField[String] = Setter[String] {
      case UpdaterInput(e: MyEntity, valueOpt, input) if input.items.contains("Bond") =>
        e.name = valueOpt.getOrElse("(none)") + input.items.mkString("-", "-", "")
    }
    val entity = new MyEntity()
    field.updater.isDefinedAt(UpdaterInput(entity, Some("James"), GetterInput.single("Dean"))) must be (false)
  }
}
