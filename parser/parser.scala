package pytanie.parser

object &: :
  def unapply[T](tokens: LazyList[Token[T]]): Option[(T, LazyList[Token[T]])] =
    tokens match
      case head #:: tail => Some((head.data, tail))
      case _             => None

type Parser[T, E] = PartialFunction[Tokens[T], (Tokens[T], E)]

type Tupled[A] <: Tuple = A match
  case Tuple => A & Tuple
  case _     => A *: EmptyTuple

private def tupled[A](a: A): Tupled[A] = a match
  case t: Tuple => t
  case _        => a *: EmptyTuple

def just[T](t: T): Parser[T, T] =
  case `t` &: tail => (tail, t)

extension [T, E](p: Parser[T, E])
  def <+>[F](r: Parser[T, F]): Parser[T, Tuple.Concat[Tupled[E], Tupled[F]]] =
    def parse(
        input: Tokens[T]
    ): Option[(Tokens[T], Tuple.Concat[Tupled[E], Tupled[F]])] =
      for
        (tailP, resP) <- p.lift(input)
        (tailR, resR) <- r.lift(tailP)
      yield (tailR, tupled(resP) ++ tupled(resR))
    parse.unlift

  def map[F](f: E => F): Parser[T, F] =
    p.andThen:
      case (tail, res) => (tail, f(res))

  def +>[F](r: Parser[T, F]): Parser[T, F] = p.andThen(_._1).andThen(r)

  def <+[F](r: Parser[T, F]): Parser[T, E] =
    def parse(input: Tokens[T]): Option[(Tokens[T], E)] =
      for
        (tailP, res) <- p.lift(input)
        (tailR, _) <- r.lift(tailP)
      yield (tailR, res)
    parse.unlift

  def <|>[F](r: Parser[T, F]): Parser[T, E | F] = p.orElse(r)
