package com.github.triangle

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner


/** A behavior specification for [[com.github.triangle.Field]].
  * @author Eric Pabst (epabst@gmail.com)
  */
@RunWith(classOf[JUnitRunner])
class DelegatingPortableFieldSpec extends BaseFieldContractSpec {
  //required by contract spec
  def toBaseField[T](field: PortableField[T]) = new Field(field)

  //tests are inherited
}
