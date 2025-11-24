package typelevelnumbers.ternary

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

  override def equals(obj: Any): Boolean =
    import compiletime.asMatchable
    obj.asMatchable match
      case that: WholeNumber[?] => this.eq(that) || this.trits == that.trits
      case _ => false

  override lazy val hashCode: Int = trits.hashCode

  override def toString: String = s"WholeNumber($trits)"
end WholeNumber

object WholeNumber:
  def apply[Encoding <: Trits.OfNumber](trits: Encoding): WholeNumber[Encoding] =
    NaiveImpl(trits)

  inline def apply[Encoding <: Trits.OfNumber]: WholeNumber[Encoding] =
    BitIntImpl(compiletime.constValue[Encoding].toBigInt)

  def unapply[Encoding <: Trits.OfNumber](number: WholeNumber[Encoding]): Tuple1[Encoding] =
    Tuple1(number.trits)

  type Negated[N <: WholeNumber[? <: Trits.OfNumber]] <: WholeNumber[? <: Trits.OfNumber] =
    N match
      case WholeNumber[trits] => WholeNumber[Trits.OfNumber.Negated[trits]]

  private final case class NaiveImpl[Encoding <: Trits.OfNumber](trits: Encoding) extends WholeNumber[Encoding]

  private final class BitIntImpl[Encoding <: Trits.OfNumber](value: BigInt) extends WholeNumber[Encoding]:
    def trits: Encoding = Trits.fromBigInt(value).asInstanceOf[Encoding]
  end BitIntImpl
end WholeNumber
