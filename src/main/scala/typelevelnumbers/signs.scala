package typelevelnumbers

import scala.annotation.targetName

/**
 * Ternary type representing a sign:
 *  - [[N]] for Negative
 *  - [[Z]] for Neutral
 *  - [[P]] for Positive
 */
sealed trait Trit:
  @targetName("negated") def unary_- : Trit.Negated[this.type]
object Trit:
  type Z = Z.type
  case object Z extends Trit:
    @targetName("negated") def unary_- : Z = Z
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
end Trit

/**
 * Binary type representing a sign:
 *  - [[N]] for Negative
 *  - [[P]] for Positive
 */
sealed trait SignBit extends Trit:
  @targetName("negated") def unary_- : SignBit.Negated[this.type] & Trit.Negated[this.type]
object SignBit:
  type N = SignBit.N.type
  case object N extends SignBit:
    @targetName("negated") def unary_- : P = P
  end N

  type P = SignBit.P.type
  case object P extends SignBit:
    @targetName("negated") def unary_- : N = N
  end P

  type Negated[T <: SignBit] <: SignBit = T match
    case N => P
    case P => N

  def negated[T <: SignBit](trit: T): Negated[T] = trit match
    case _: N => P
    case _: P => N
end SignBit
