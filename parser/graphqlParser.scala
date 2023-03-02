package pytanie.parser

import pytanie.model.*

private type ParserS[E] = Parser[String, E]

def parseQuery(input: String): Option[Query] =
  println(input)
  query.lift(graphqlTokenizer(input)).map(_._2)

private def query: ParserS[Query] =
  ((just("query") +> identifier.?).? <+> selectionSet)
    .map: (name, set) =>
      Query(name.flatten, set)

private def selectionSet: ParserS[SelectionSet] =
  (just("{") +> field.* <+ just("}"))
    .map(SelectionSet(_))

private def field: ParserS[Field] =
  (identifier <+> arguments.? <+> selectionSet.?)
    .map: (name, args, set) =>
      Field(name, args, set)

private def arguments: ParserS[Arguments] =
  (just("(") +> argument.separatedBy(just(",")) <+ just(")")).map(Arguments(_))

private def argument: ParserS[Argument] =
  ((identifier <+ just(":")) <+> value).map: (name, value) =>
    Argument(name, value)

private def value: ParserS[Value] = intValue <|> stringValue

private def intValue: ParserS[IntValue] = int.map(IntValue(_))

private def stringValue: ParserS[StringValue] =
  case head &: tail if head(0) == '"' =>
    (tail, StringValue(head.drop(1).dropRight(1)))

private def identifier: ParserS[String] =
  case head &: tail if head(0).isLetter => (tail, head)

private def int: ParserS[Int] =
  case num &: tail if num.forall(_.isDigit) => (tail, num.toInt)
