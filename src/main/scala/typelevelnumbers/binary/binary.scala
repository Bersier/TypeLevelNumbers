package typelevelnumbers.binary

import typelevelnumbers.ternary.Trit

import scala.annotation.tailrec
import scala.compiletime.constValue
import scala.compiletime.ops.int./

/**
 * Sequence of [[Bit]]s
 */
sealed trait Bits derives CanEqual:

  final def reversed: Bits =
    Bits.reversed(this, Bits.None)

  def withoutTrailingZeros: Bits

  /**
   * The bits are interpreted as a number expressed in little-endian binary.
   */
  final def toBigInt: BigInt =
    @tailrec def toBigInt(bits: Bits, acc: BigInt): BigInt = bits match
      case Bits.None => acc
      case Bits.Some(first, rest) =>
        toBigInt(rest, acc * 2 + first.toInt)
    toBigInt(this.reversed, BigInt(0))

  final override def toString: String =
    val builder = new StringBuilder("")
    @tailrec def toString(trits: Bits): String = trits match
      case Bits.None => builder.toString
      case Bits.Some(first, rest) =>
        val digit = first match
          case Bit.O => '0'
          case Bit.I => 'I'
        builder.append(digit)
        toString(rest)
    toString(this)

object Bits:
  type None = None.type
  case object None extends Bits:
    def withoutTrailingZeros: Bits = None
  final case class Some[+First <: Bit, Rest <: Bits](first: First, rest: Rest) extends Bits:
    def withoutTrailingZeros: Bits =
      val trimmedR = rest.withoutTrailingZeros
      (first, trimmedR) match
        case (Bit.O, None) => None
        case _ => Some(first, trimmedR)

  @tailrec private def reversed(bits: Bits, acc: Bits): Bits = bits match
    case None => acc
    case Some(first, rest) => reversed(rest, Some(first, acc))

  /**
   * The non-negative number is converted to its little-endian binary encoding.
   */
  def fromBigInt(n: BigInt): Bits = if n == 0 then None else
    assert(n > 0)
    @tailrec def fromBigInt(n: BigInt, acc: Bits): Bits =
      if n == 0 then acc else
        val quotient = n / 2
        val remainder = n.mod(2).toInt match
          case 0 => Bit.O
          case _ => Bit.I
        fromBigInt(quotient, Some(remainder, acc))
    fromBigInt(n, None).reversed

  type FromInt[N <: Int] <: Bits = N match
    case 0 => None
    case _ => Some[Bit.FromInt[N], FromInt[N / 2]]

  inline def fromInt[N <: Int](n: N): FromInt[N] = n match
    case _: 0 => None
    case _ => Some(Bit.fromInt(n), fromInt(constValue[N / 2]))

  type Compared[B1 <: Bits, B2 <: Bits] <: Trit = (B1, B2) match
    case (None, _) => IsZero[B2] match
      case false => Trit.N
      case true => Trit.Z
    case (_, None) => IsZero[B1] match
      case false => Trit.P
      case true => Trit.Z
    case (Some[f1, r1], Some[f2, r2]) => Compared[r1, r2] match
      case Trit.Z => (f1, f2) match
        case (Bit.O, Bit.O) => Trit.Z
        case (Bit.O, Bit.I) => Trit.N
        case (Bit.I, Bit.O) => Trit.P
        case (Bit.I, Bit.I) => Trit.Z
      case Trit.N => Trit.N
      case Trit.P => Trit.P

  type IsZero[B <: Bits] <: Boolean = B match
    case None => true
    case Some[Bit.O, r] => IsZero[r]
    case Some[Bit.I, r] => false

  type Product[B1 <: Bits, B2 <: Bits] <: Bits = (B1, B2) match
    case (None, _) => None
    case (_, None) => None
    case (Some[Bit.I, None], _) => B2
    case (_, Some[Bit.I, None]) => B1
    case (Some[Bit.O, r1], _) => Some[Bit.O, Product[r1, B2]]
    case (_, Some[Bit.O, r2]) => Some[Bit.O, Product[B1, r2]]
    case (Some[Bit.I, r1], Some[Bit.I, r2]) => Some[Bit.I, Sum[Sum[r1, r2], Some[Bit.O, Product[r1, r2]]]]

  type Sum[B1 <: Bits, B2 <: Bits] <: Bits = (B1, B2) match
    case (b1, b2) => SumC[b1, b2, Bit.O]

  type SumC[B1 <: Bits, B2 <: Bits, C <: Bit] <: Bits = (B1, B2) match
    case (None, _) => WithAddedBit[B2, C]
    case (_, None) => WithAddedBit[B1, C]
    case (Some[f1, r1], Some[f2, r2]) => Bit.Sum[f1, f2, Bit.I] match
        case (carry, bit) => Some[bit, SumC[r1, r2, carry]]

  type WithAddedBit[B <: Bits, C <: Bit] <: Bits = B match
    case None => AsBits[C]
    case Some[f, r] => Bit.Sum[f, C, Bit.O] match
      case (Bit.O, bit) => Some[bit, r]
      case (Bit.I, Bit.O) => Some[Bit.O, WithAddedBit[r, Bit.I]]

  type AsBits[B <: Bit] <: Bits = B match
      case Bit.O => None
      case Bit.I => Some[Bit.I, None]
end Bits
