package com.github.triangle

import com.github.triangle.PortableField._
import org.scalatest.Spec

/** A Specification for [[com.github.triangle.Transformer]].
  * @author Eric Pabst (epabst@gmail.com)
  */
class TransformerSpec extends Spec {
  it("must provide a convenient clearer") {
    Transformer((string: String) => (v: String) => string + v, noTransformerForEmpty)
  }
}