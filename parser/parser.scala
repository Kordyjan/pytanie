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

def nil[T]: Parser[T, Nil.type] =
  case t => (t, Nil)

extension [T, E](p: Parser[T, E])
  private[parser] def next[F](r: => Parser[T, F]): Parser[T, (E, F)] =
    def parse(input: Tokens[T]): Option[(Tokens[T], (E, F))] =
      for
        (tailP, resP) <- p.lift(input)
        (tailR, resR) <- r.lift(tailP)
      yield (tailR, (resP, resR))
    parse.unlift

  def <+>[F](r: Parser[T, F]): Parser[T, Tuple.Concat[Tupled[E], Tupled[F]]] =
    p.next(r).map((e, f) => tupled(e) ++ tupled(f))

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

  def <|>[F](r: => Parser[T, F]): Parser[T, E | F] = p.orElse(r)

  def ? : Parser[T, Option[E]] =
    p.andThen((t, res) => (t, Some(res))).orElse((_, None))

  def * : Parser[T, List[E]] = p.+ <|> nil

  def + : Parser[T, List[E]] = p.next(p.*).map((e, f) => e :: f)

  def separatedBy[F](
      sep: Parser[T, F],
      trailingOpt: Boolean = false
  ): Parser[T, List[E]] =
    val list = (p.next((sep +> p).*))
    val listWithEnd = if trailingOpt then (list <+ sep.?).? else list.?
    listWithEnd.map:
      case Some(h, t) => h :: t
      case None       => Nil

extension (context: StringContext)
  def lit: Parser[String, String] = just(context.parts.head)