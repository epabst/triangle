package com.github.triangle

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import actors.threadpool.AtomicInteger
import collection.mutable

/** A behavior specification for [[com.github.triangle.PartialDelegatingField]].
  * @author Eric Pabst (epabst@gmail.com)
  */
@RunWith(classOf[JUnitRunner])
class PartialDelegatingFieldSpec extends BaseFieldContractSpec {
  case class AnyRefHolder[S <: AnyRef](ref: S)

  //required by contract spec
  def toBaseField[T](field: PortableField[T]) = new PartialDelegatingField[T] {
    // used for deepCollect
    protected val delegate = field

    protected val subjectGetter: PartialFunction[AnyRef,AnyRef] = {
      case ref => ref
    }
  }

  it("must unwrap the AnyRef for getter") {
    val field = new PartialDelegatingField[Int] {
      protected val delegate = PortableField.default(5)

      protected val subjectGetter: PartialFunction[AnyRef,AnyRef] = {
        case AnyRefHolder(ref) => ref
      }
    }
    val holder = AnyRefHolder(PortableField.UseDefaults)
    field.getValue(holder) must be (Some(5))
  }

  it("must unwrap the AnyRef for setter") {
    val field = new PartialDelegatingField[Int] {
      protected val delegate = PortableField.mapField[Int]("count")

      protected val subjectGetter: PartialFunction[AnyRef,AnyRef] = {
        case AnyRefHolder(ref) => ref
      }
    }
    val holder = AnyRefHolder(mutable.Map.empty[String,Any])
    field.updateWithValue(holder, Some(10))
    holder.ref.get("count") must be (Some(10))
  }

  it("must unwrap the AnyRef for updater (using setter)") {
    val field = new PartialDelegatingField[Int] {
      protected val delegate = PortableField.mapField[Int]("count")

      protected val subjectGetter: PartialFunction[AnyRef,AnyRef] = {
        case AnyRefHolder(ref) => ref
      }
    }
    val holder = AnyRefHolder(mutable.Map.empty[String,Any])
    field.updateWithValue(holder, Some(10))
    holder.ref.get("count") must be (Some(10))
  }

  it("must unwrap the AnyRef for getterFromItem") {
    val field = new PartialDelegatingField[String] {
      protected def delegate = fieldWithGetterFromItem

      protected val subjectGetter: PartialFunction[AnyRef,AnyRef] = {
        case AnyRefHolder(ref) => ref
      }
    }
    field.getter(GetterInput(AnyRefHolder("hello"), "ignored", AnyRefHolder(Set(1,0,3)))) must be (Some("hello4"))
  }

  it("must unwrap the AnyRef for setterUsingItem") {
    val field = new PartialDelegatingField[Int] {
      protected def delegate = fieldWithSetterUsingContext

      protected val subjectGetter: PartialFunction[AnyRef,AnyRef] = {
        case AnyRefHolder(ref) => ref
      }
    }
    val holder = AnyRefHolder(new AtomicInteger(0))
    field.updateWithValue(holder, Some(10), GetterInput.single(Set(1,0,3)))
    holder.ref.get() must be (14)
  }
}
