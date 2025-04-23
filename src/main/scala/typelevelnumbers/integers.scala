package typelevelnumbers

import scala.annotation.targetName

trait WholeNumber[Encoding <: Trits.OfNumber]:
  def trits: Encoding
  @targetName("negated") def unary_- : WholeNumber.Negated[this.type] = this match
    case n: WholeNumber[?] => n match
      case WholeNumber(trits) => WholeNumber(Trits.OfNumber.negated(trits))
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

sealed trait Trits
object Trits:
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

  sealed trait NonEmpty[First <: Trit, Rest <: Trits] extends Trits:
    def first: First
    def rest: Rest
  object NonEmpty:
    def apply[First <: Trit, Rest <: Trits]
    (first: First, rest: Rest): NonEmpty[First, Rest] =
      GenericNonEmpty(first, rest)
    def unapply[First <: Trit, Rest <: Trits]
    (trits: NonEmpty[First, Rest]): (First, Rest) = trits match
      case GenericNonEmpty(first, rest) => (first, rest)
      case WithSignBit(first, rest) => (first, rest)
    private case class GenericNonEmpty[First <: Trit, Rest <: Trits]
    (first: First, rest: Rest) extends NonEmpty[First, Rest]
  end NonEmpty

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
end Trits

sealed trait Trit:
  @targetName("negated") def unary_- : Trit.Negated[this.type]
object Trit:
  type Z = Z.type
  case object Z extends Trit:
    @targetName("negated") def unary_- : Z = Z
  end Z

  type N1 = SignBit.N1
  val N1: N1 = SignBit.N1

  type P1 = SignBit.P1
  val P1: P1 = SignBit.P1

  type Negated[T <: Trit] <: Trit = T match
    case Z  => Z
    case N1 => P1
    case P1 => N1

  def negated[T <: Trit](trit: T): Negated[T] = trit match
    case _: Z  => Z
    case _: N1 => P1
    case _: P1 => N1
end Trit

sealed trait SignBit extends Trit:
  @targetName("negated") def unary_- : SignBit.Negated[this.type] & Trit.Negated[this.type]
object SignBit:
  type N1 = SignBit.N1.type
  case object N1 extends SignBit:
    @targetName("negated") def unary_- : P1 = P1
  end N1

  type P1 = SignBit.P1.type
  case object P1 extends SignBit:
    @targetName("negated") def unary_- : N1 = N1
  end P1

  type Negated[T <: SignBit] <: SignBit = T match
    case N1 => P1
    case P1 => N1

  def negated[T <: SignBit](trit: T): Negated[T] = trit match
    case _: N1 => P1
    case _: P1 => N1
end SignBit
