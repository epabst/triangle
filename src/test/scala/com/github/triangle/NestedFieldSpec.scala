package com.github.triangle

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import actors.threadpool.AtomicInteger
import collection.mutable

/** A behavior specification for [[com.github.triangle.NestedField]].
  * @author Eric Pabst (epabst@gmail.com)
  */
@RunWith(classOf[JUnitRunner])
class NestedFieldSpec extends BaseFieldContractSpec {
  case class AnyRefHolder[S <: AnyRef](ref: S)

  //required by contract spec
  def toBaseField[T](field: PortableField[T]) = new NestedField[T](Getter.single({
    case ref => Some(ref)
  }), field)

  it("must unwrap the AnyRef for getter") {
    val field = new NestedField[Int](Getter.single {
      case AnyRefHolder(ref) => Some(ref)
    }, PortableField.default(5))
    val holder = AnyRefHolder(PortableField.UseDefaults)
    field.getValue(holder) must be (Some(5))
  }

  it("must unwrap the AnyRef for setter") {
    val field = new NestedField[Int](Getter.single {
      case AnyRefHolder(ref) => Some(ref)
    }, PortableField.mapField[Int]("count"))
    val holder = AnyRefHolder(mutable.Map.empty[String,Any])
    field.updateWithValue(holder, Some(10))
    holder.ref.get("count") must be (Some(10))
  }

  it("must unwrap the AnyRef for updater (using setter)") {
    val field = new NestedField[Int](Getter.single {
      case AnyRefHolder(ref) => Some(ref)
    }, PortableField.mapField[Int]("count"))
    val holder = AnyRefHolder(mutable.Map.empty[String,Any])
    field.updateWithValue(holder, Some(10))
    holder.ref.get("count") must be (Some(10))
  }

  it("must unwrap the AnyRef for each GetterInput") {
    val field = new NestedField[String](Getter.single {
      case AnyRefHolder(ref) => Some(ref)
    }, StringIdentityField)
    field.getter(GetterInput("ignored", AnyRefHolder("hello"), "ignored")) must be (Some("hello"))
  }

  it("must unwrap the AnyRef for each UpdaterInput") {
    val field = new NestedField[Int](Getter.single {
      case AnyRefHolder(ref) => Some(ref)
    }, fieldWithSetterUsingContext)
    val holder = AnyRefHolder(new AtomicInteger(0))
    field.updateWithValue(holder, Some(10), GetterInput("ignored", Set(1,0,3)))
    holder.ref.get() must be (14)
  }
}
