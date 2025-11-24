package typelevelnumbers.binary

import scala.compiletime.ops.int.+

sealed trait Bit derives CanEqual:
  def toInt: Bit.ToInt[this.type]
object Bit:
  type O = O.type
  case object O extends Bit:
    def toInt: 0 = 0
  end O

  type I = I.type
  case object I extends Bit:
    def toInt: 1 = 1
  end I

  type Sum[B1 <: Bit, B2 <: Bit, B3 <: Bit] <: (Bit, Bit) = ToInt[B1] + ToInt[B2] + ToInt[B3] match
    case 0 => (O, O)
    case 1 => (O, I)
    case 2 => (I, O)
    case 3 => (I, I)

  type ToInt[B <: Bit] <: Int = B match
    case O => 0
    case I => 1

  type FromInt[N <: 0 | 1] <: Bit = N match
    case 0 => O
    case 1 => I

  def fromInt[N <: 0 | 1](n: N): FromInt[N] = n match
    case _: 0 => O
    case _: 1 => I
end Bit
