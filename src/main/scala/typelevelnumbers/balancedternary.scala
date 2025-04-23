package typelevelnumbers

/**
 * Sequence of [[Trit]]s
 */
sealed trait Trits
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
end Trits
