package pytanie.parser

def tokenize[T](
    procedure: (String, Int) => Option[Token[T]]
)(input: String): LazyList[Token[T]] =
  LazyList.unfold(0): pos =>
    procedure(input, pos).map: token =>
      (token, token.end)

case class Token[T](data: T, start: Int, end: Int)
