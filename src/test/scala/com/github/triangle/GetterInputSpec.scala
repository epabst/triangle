package com.github.triangle

import org.scalatest.FunSpec
import org.scalatest.matchers.MustMatchers

/**
 * A specification for [[com.github.triangle.GetterInput]].
 * @author Eric Pabst (epabst@gmail.com)
 * Date: 10/9/12
 * Time: 6:35 AM
 */
class GetterInputSpec extends FunSpec with MustMatchers {
  it("must not allow nested GetterInputs") {
    intercept[IllegalArgumentException] {
      GetterInput.single(GetterInput.single("a string"))
    }.getMessage must include ("nested")
  }
}
