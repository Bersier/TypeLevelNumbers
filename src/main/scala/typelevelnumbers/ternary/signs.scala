package typelevelnumbers.ternary

import scala.annotation.targetName
import scala.compiletime.constValue
import scala.compiletime.ops.int.+

/**
 * Ternary type representing a sign:
 *  - [[N]] for Negative
 *  - [[Z]] for Neutral
 *  - [[P]] for Positive
 */
sealed trait Trit derives CanEqual:
  @targetName("negated") def unary_- : Trit.Negated[this.type]
  def toInt: Trit.ToInt[this.type ]
object Trit:
  type Z = Z.type
  case object Z extends Trit:
    @targetName("negated") def unary_- : Z = Z
    def toInt: 0 = 0
  end Z

  type N = SignBit.N
  val N: N = SignBit.N

  type P = SignBit.P
  val P: P = SignBit.P

  type Negated[T <: Trit] <: Trit = T match
    case Z => Z
    case N => P
    case P => N

  def negated[T <: Trit](trit: T): Negated[T] = trit match
    case _: Z => Z
    case _: N => P
    case _: P => N

  type Sum3[T1 <: Trit, T2 <: Trit, T3 <: Trit] <: (Trit, Trit) = ToInt[T1] + ToInt[T2] + ToInt[T3] match
    case -3 => (N, Z)
    case -2 => (N, P)
    case -1 => (Z, N)
    case 0 => (Z, Z)
    case 1 => (Z, P)
    case 2 => (P, N)
    case 3 => (P, Z)

  type Sum[T1 <: Trit, T2 <: Trit] <: (Trit, Trit) = ToInt[T1] + ToInt[T2] match
    case -2 => (N, P)
    case -1 => (Z, N)
    case 0 => (Z, Z)
    case 1 => (Z, P)
    case 2 => (P, N)

  inline def sum[T1 <: Trit, T2 <: Trit](t1: T1, t2: T2): Sum[T1, T2] = constValue[ToInt[T1] + ToInt[T2]] match
    case _: -2 => (N, P)
    case _: -1 => (Z, N)
    case _: 0 => (Z, Z)
    case _: 1 => (Z, P)
    case _: 2 => (P, N)

  type ToInt[T <: Trit] <: Int = T match
    case Z => 0
    case N => -1
    case P => 1

  def sign(n: BigInt): Trit =
    if n < 0 then N else
    if n > 0 then P
    else Z
end Trit

/**
 * Binary type representing a sign:
 *  - [[N]] for Negative
 *  - [[P]] for Positive
 */
sealed trait SignBit extends Trit derives CanEqual:
  @targetName("negated") def unary_- : SignBit.Negated[this.type] & Trit.Negated[this.type]
object SignBit:
  type N = SignBit.N.type
  case object N extends SignBit:
    @targetName("negated") def unary_- : P = P
    def toInt: -1 = -1
  end N

  type P = SignBit.P.type
  case object P extends SignBit:
    @targetName("negated") def unary_- : N = N
    def toInt: 1 = 1
  end P

  type Negated[T <: SignBit] <: SignBit = T match
    case N => P
    case P => N

  def negated[T <: SignBit](trit: T): Negated[T] = trit match
    case _: N => P
    case _: P => N

  def sign(n: BigInt): Option[SignBit] =
    if n < 0 then Some(N) else
    if n > 0 then Some(P)
    else None
end SignBit
