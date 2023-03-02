package pytanie.parser

import pytanie.model.*

private type ParserS[E] = Parser[String, E]

def parseQuery(input: String): Option[Query] =
  println(input)
  query.lift(graphqlTokenizer(input)).map(_._2)

def query: ParserS[Query] =
  ((just("query") +> identifier.?).? <+> selectionSet)
    .map: (name, set) =>
      Query(name.flatten, set)

def selectionSet: ParserS[SelectionSet] =
  (just("{") +> field.* <+ just("}"))
    .map(SelectionSet(_))

def field: ParserS[Field] =
  (identifier <+> selectionSet.?)
    .map: (name, set) =>
      Field(name, set)

def identifier: ParserS[String] =
  case head &: tail if head(0).isLetter => (tail, head)
