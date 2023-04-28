package mipt.examples

import mipt.optics.Iso
import mipt.optics.Iso.*
import mipt.optics.Iso.given

object Question1_Iso:
  case class WrappedInt(value: Int)
  def increase(a: Int): Int = a + 1

  val myInt = WrappedInt(42)
  // increase(myInt) - ERROR

object Example1_Iso:
  case class WrappedInt(value: Int)
  def increase(a: Int): Int = a + 1

  val myInt = WrappedInt(42)
  def increaseWrapped(a: WrappedInt): WrappedInt = WrappedInt(a.value + 1)
  def increaseWrapped2(a: WrappedInt): WrappedInt = WrappedInt(increase(a.value))

  def applyFunctionToWrapped(f: Int => Int)(a: WrappedInt): WrappedInt = WrappedInt(f(a.value)) // how to apply to Int => A; (Int, A) => C etc
  // lift: (A => B) => (F[A] => F[B])

  @main def e1: Unit =
    val wrapped = WrappedInt(42)

    println(applyFunctionToWrapped(_ + 1)(wrapped))
    println(applyFunctionToWrapped(_ - 1)(wrapped))

  @main def e1_2: Unit =
    given Iso[WrappedInt, Int] = Iso[WrappedInt, Int](_.value, WrappedInt.apply)
    val wrapped = WrappedInt(42)

    println(increase(wrapped.to[Int]).to[WrappedInt])
    println(Iso[WrappedInt, Int].update(increase)(wrapped))

  @main def e1_3: Unit =
    case class ThirdInt(value: Int)

    given Iso[WrappedInt, Int] = Iso[WrappedInt, Int](_.value, WrappedInt.apply)
    given Iso[ThirdInt, Int] = Iso[ThirdInt, Int](_.value, ThirdInt.apply)
    given Iso[WrappedInt, ThirdInt] = Iso[WrappedInt, Int].compose(Iso[Int, ThirdInt])

    val wrapped = WrappedInt(42)
    println(wrapped.to[ThirdInt])
