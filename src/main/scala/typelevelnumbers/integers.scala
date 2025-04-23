package typelevelnumbers

import scala.annotation.targetName

/**
 * Integer
 *
 * @tparam Encoding smallest balanced ternary encoding of this integer
 */
trait WholeNumber[Encoding <: Trits.OfNumber]:
  /**
   * @return the smallest balanced ternary encoding of this integer
   */
  def trits: Encoding

  /**
   * @return the negation of this integer
   */
  @targetName("negated") def unary_- : WholeNumber.Negated[this.type] = this match
    case n: WholeNumber[?] => n match
      case WholeNumber(trits) => WholeNumber(Trits.OfNumber.negated(trits))
end WholeNumber

object WholeNumber:
  def apply[Encoding <: Trits.OfNumber](trits: Encoding): WholeNumber[Encoding] =
    Implementation(trits)

  def unapply[Encoding <: Trits.OfNumber](number: WholeNumber[Encoding]): Tuple1[Encoding] =
    Tuple1(number.trits)

  type Negated[N <: WholeNumber[? <: Trits.OfNumber]] <: WholeNumber[? <: Trits.OfNumber] =
    N match
      case WholeNumber[trits] => WholeNumber[Trits.OfNumber.Negated[trits]]

  private final case class Implementation[Encoding <: Trits.OfNumber](
    trits: Encoding,
  ) extends WholeNumber[Encoding]
end WholeNumber
