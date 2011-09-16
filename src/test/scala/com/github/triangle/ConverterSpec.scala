package com.github.triangle

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.matchers.MustMatchers
import org.scalatest.Spec
import Converter._

/**
 * A behavior specification for {@link Converter}.
 * @author Eric Pabst (epabst@gmail.com)
 * Date: 9/15/11
 * Time: 7:59 PM
 */

@RunWith(classOf[JUnitRunner])
class ConverterSpec extends Spec with MustMatchers {
  describe("anyToString") {
    it("must use the Object.toString method") {
      val from = new Object
      anyToString.convertTo(from) must be (Some(from.toString))
    }

    it("must work for primitive types") {
      anyToString.convertTo(1) must be (Some("1"))
      anyToString.convertTo(1.5) must be (Some("1.5"))
      anyToString.convertTo(true) must be (Some("true"))
      anyToString.convertTo('a') must be (Some("a"))
    }
  }

  describe("stringToAnyVal") {
    it("must convert between primitive types") {
      stringToAnyVal.convertTo[Int]("1") must be (Some(1))
      stringToAnyVal.convertTo[Long]("123") must be (Some(123L))
      stringToAnyVal.convertTo[Int]("123") must be (Some(123))
      stringToAnyVal.convertTo[Short]("123") must be (Some(123))
      stringToAnyVal.convertTo[Byte]("123") must be (Some(123))
      stringToAnyVal.convertTo[Double]("3232.11") must be (Some(3232.11))
      stringToAnyVal.convertTo[Float]("2.3") must be (Some(2.3f))
      stringToAnyVal.convertTo[Boolean]("true") must be (Some(true))
    }

    it("must return None if unable to parse") {
      stringToAnyVal.convertTo[Int]("foo") must be (None)
      stringToAnyVal.convertTo[Long]("foo") must be (None)
      stringToAnyVal.convertTo[Int]("foo") must be (None)
      stringToAnyVal.convertTo[Short]("foo") must be (None)
      stringToAnyVal.convertTo[Byte]("foo") must be (None)
      stringToAnyVal.convertTo[Double]("foo") must be (None)
      stringToAnyVal.convertTo[Float]("foo") must be (None)
      stringToAnyVal.convertTo[Boolean]("foo") must be (None)
    }
  }

  describe("stringToCurrency") {
    it("must parse various number formats") {
      stringToCurrency.convert("$1.00").get must be(1.0)
      stringToCurrency.convert("$1").get must be(1.0)
      stringToCurrency.convert("1.00").get must be(1.0)
      stringToCurrency.convert("1").get must be(1.0)
      stringToCurrency.convert("-1.00").get must be(-1.0)
      stringToCurrency.convert("-1").get must be(-1.0)
      stringToCurrency.convert("($1.00)").get must be(-1.0)
      stringToCurrency.convert("($1)").get must be(-1.0)
      //do these later if desired
      stringToCurrency.convert("(1.00)") must be(None)
      stringToCurrency.convert("(1)") must be(None)
      stringToCurrency.convert("-$1.00") must be(None)
      stringToCurrency.convert("-$1") must be(None)
    }
  }

  describe("currencyToString") {
    it("must format correctly") {
      currencyToString.convert(1234.2) must be (Some("$1,234.20"))
      currencyToString.convert(1234.22324) must be (Some("$1,234.22"))
    }
  }

  describe("currencyToEditString") {
    it("must format correctly") {
      currencyToEditString.convert(1234.2) must be (Some("1,234.20"))
      currencyToEditString.convert(1234.22324) must be (Some("1,234.22"))
    }
  }

  describe("stringToEnum") {
    object MyEnum extends Enumeration {
      val A = Value("A")
      val B = Value("B")
    }
    val converter = stringToEnum(MyEnum)

    it("must convert") {
      converter.convertTo[MyEnum.Value]("A") must be (Some(MyEnum.A))
      converter.convertTo[MyEnum.Value]("B") must be (Some(MyEnum.B))
    }

    it("must return None if unable to convert") {
      converter.convertTo[MyEnum.Value]("C") must be (None)
      converter.convertTo[MyEnum.Value]("") must be (None)
    }
  }
}
