package pytanie.parser

type Tokens[T] = LazyList[Token[T]]

def tokenize[T](
    procedure: (String, Int) => Option[Token[T]]
)(input: String): Tokens[T] =
  LazyList.unfold(0): pos =>
    procedure(input, pos).map: token =>
      (token, token.end)

case class Token[T](data: T, start: Int, end: Int)
