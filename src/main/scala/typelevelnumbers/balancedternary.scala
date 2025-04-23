package typelevelnumbers

import scala.annotation.tailrec

/**
 * Sequence of [[Trit]]s
 */
sealed trait Trits derives CanEqual:
  def toBigInt: BigInt =
    @tailrec def toBigInt(trits: Trits, acc: BigInt): BigInt = trits match
      case Trits.Empty => acc
      case Trits.NonEmpty(first, rest) =>
        toBigInt(rest, acc * 3 + first.toInt)
    toBigInt(this, BigInt(0))

  override def toString: String =
    val builder = new StringBuilder("")
    @tailrec def toString(trits: Trits): String = trits match
      case Trits.Empty => builder.toString
      case Trits.NonEmpty(first, rest) =>
        val digit = first match
          case Trit.N => '-'
          case Trit.Z => '0'
          case Trit.P => '+'
        builder.append(digit)
        toString(rest)
    toString(this)
object Trits:
  /**
   * Sequence of [[Trit]]s that starts with [[SignBit]],
   * and can therefore be used as a canonical representation of an integer
   */
  sealed trait OfNumber extends Trits
  object OfNumber:
    type Negated[T <: OfNumber] <: OfNumber = T match
      case Empty => Empty
      case WithSignBit[first, rest] =>
        WithSignBit.Negated[WithSignBit[first, rest]]

    def negated[T <: OfNumber](trits: T): Negated[T] = trits match
      case _: Empty => Empty
      case trits: WithSignBit[?, ?] => WithSignBit.negated(trits)
  end OfNumber

  type Empty = Empty.type
  case object Empty extends OfNumber

  sealed trait NonEmpty[+First <: Trit, Rest <: Trits] extends Trits:
    def first: First
    def rest: Rest

  object NonEmpty:
    def apply[First <: Trit, Rest <: Trits](first: First, rest: Rest): NonEmpty[First, Rest] =
      first match
        case first: SignBit => WithSignBit(first, rest)
        case _: Trit.Z => WithZ(rest).asInstanceOf[NonEmpty[First, Rest]]

    def unapply[First <: Trit, Rest <: Trits](trits: NonEmpty[First, Rest]): (First, Rest) = trits match
      case WithZ(rest) => (Trit.Z, rest)
      case WithSignBit(first, rest) => (first, rest)

    private final case class WithZ[Rest <: Trits](rest: Rest) extends NonEmpty[Trit.Z, Rest]:
      def first: Trit.Z = Trit.Z
    end WithZ
  end NonEmpty

  /**
   * [[NonEmpty]] sequence of [[Trit]]s that starts with a [[SignBit]].
   */
  final case class WithSignBit[First <: SignBit, Rest <: Trits]
  (first: First, rest: Rest) extends OfNumber, NonEmpty[First, Rest]
  object WithSignBit:
    type Negated[T <: WithSignBit[?, ?]] <: WithSignBit[?, ?] = T match
      case WithSignBit[first, rest] =>
        WithSignBit[SignBit.Negated[first], Trits.Negated[rest]]

    def negated[T <: WithSignBit[?, ?]](trits: T): Negated[T] = trits match
      case trits: WithSignBit[?, ?] => trits match
        case WithSignBit(first, rest) =>
          WithSignBit(SignBit.negated(first), Trits.negated(rest))
  end WithSignBit

  type Negated[T <: Trits] <: Trits = T match
    case Empty => Empty
    case NonEmpty[first, rest] =>
      NonEmpty[Trit.Negated[first], Negated[rest]]

  def negated[T <: Trits](trits: T): Negated[T] = trits match
    case _: Empty => Empty
    case trits: NonEmpty[?, ?] => trits match
      case NonEmpty(first, rest) =>
        NonEmpty(Trit.negated(first), negated(rest))

  def fromBigInt(n: BigInt): Trits.OfNumber =
    if n == 0 then Trits.Empty else
      @tailrec def fromBigInt(n: BigInt, acc: Trits): Trits.OfNumber =
        val remainder = (n + 1).mod(3) - 1
        val quotient = (n - remainder) / 3
        if quotient == 0 then
          SignBit.sign(remainder) match
            case Some(sign) => Trits.WithSignBit(sign, acc)
            case None => throw AssertionError(s"Quotient and remainder cannot both be 0 for non-zero $n.")
        else fromBigInt(quotient, Trits.NonEmpty(Trit.sign(remainder), acc))
      fromBigInt(n, Trits.Empty)
end Trits
