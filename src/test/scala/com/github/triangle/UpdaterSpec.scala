package com.github.triangle

import com.github.triangle.PortableField._
import org.scalatest.FunSpec
import org.scalatest.junit.JUnitRunner
import org.junit.runner.RunWith
import org.scalatest.matchers.MustMatchers

/** A Specification for [[com.github.triangle.Updater]].
  * @author Eric Pabst (epabst@gmail.com)
  */
@RunWith(classOf[JUnitRunner])
class UpdaterSpec extends FunSpec with MustMatchers {
  case class MyEntity(name: String = "(none)")

  it("must provide a convenient clearer") {
    Updater[String,String]((string: String) => (v: String) => string + v, noUpdaterForEmpty[String])
  }

  describe("Updater") {
    val stringField: PortableField[String] = Updater[String] {
      case UpdaterInput(e: MyEntity, valueOpt, context) =>
        e.copy(name = valueOpt.getOrElse("(none)") + context.items.mkString("-", "-", ""))
    }

    it("must allow using the items while setting") {
      val entity = stringField.updateWithValue(new MyEntity(), Some("James"), GetterInput("Bond", "007"))
      entity.name must be ("James-Bond-007")
    }

    it("must be constructable with a partial function") {
      val field: PortableField[String] = Updater[String] {
        case UpdaterInput(e: MyEntity, valueOpt, context) if context.items.contains("Bond") =>
          e.copy(name = valueOpt.getOrElse("(none)") + context.items.mkString("-", "-", ""))
      }
      val entity = field.updateWithValue(new MyEntity(), Some("James"), GetterInput.single("Dean"))
      entity.name must be ("(none)")
    }
  }
}
