package pytanie.parser

import pytanie.model.*

private type ParserS[E] = Parser[String, E]

def parseQuery(input: String): Option[Query] =
  query.lift(graphqlTokenizer(input)).map(_._2)

private def query: ParserS[Query] =
  operationDefinition
    <|> selectionSet.map: set =>
      Query(Kind.Query, None, None, set)

private def operationDefinition: ParserS[Query] =
  (operationKind <+> identifier.? <+> varaibleDefinitions.? <+> selectionSet)
    .map: (kind, name, vars, set) =>
      Query(kind, name, vars, set)

private def operationKind: ParserS[Kind] =
  just("query").map(_ => Kind.Query) <|>
    just("mutation").map(_ => Kind.Mutation)

private def varaibleDefinitions: ParserS[VariableDefinitions] =
  (just("(") +> variableDefinition.separatedBy(just(",")) <+ just(")"))
    .map(VariableDefinitions(_))

private def variableDefinition: ParserS[VariableDefinition] =
  (just("$") +> (identifier <+ just(":")) <+> typ).map(VariableDefinition(_, _))

private def selectionSet: ParserS[SelectionSet] =
  (just("{") +> selection.* <+ just("}"))
    .map(SelectionSet(_))

private def selection: ParserS[Selection] =
  inlineFragment <|> field

private def inlineFragment: ParserS[InlineFragment] =
  (just("...") +> just("on") +> typ <+> selectionSet).map: (typ, set) =>
    InlineFragment(typ, set)

private def field: ParserS[Field] =
  (identifier <+> arguments.? <+> selectionSet.?)
    .map: (name, args, set) =>
      Field(name, args, set)

private def arguments: ParserS[Arguments] =
  (just("(") +> argument.separatedBy(just(",")) <+ just(")")).map(Arguments(_))

private def argument: ParserS[Argument] =
  ((identifier <+ just(":")) <+> value).map: (name, value) =>
    Argument(name, value)

private def value: ParserS[Value] =
  intValue <|> stringValue <|> variable <|> objectValue <|> enumValue

private def intValue: ParserS[IntValue] = int.map(IntValue(_))

private def stringValue: ParserS[StringValue] =
  case head &: tail if head(0) == '"' =>
    (tail, StringValue(head.drop(1).dropRight(1)))

private def variable: ParserS[Variable] =
  (just("$") +> identifier).map(Variable(_))

private def enumValue: ParserS[EnumValue] =
  identifier.map(EnumValue(_))

private def objectValue: ParserS[ObjectValue] =
  (just("{") +> objectField.separatedBy(just(",")) <+ just("}"))
    .map(ObjectValue(_))

private def objectField: ParserS[ObjectField] =
  ((identifier <+ just(":")) <+> value).map(ObjectField(_, _))

private def typ: ParserS[String] =
  (identifier <+> just("!").?).map: (id, nn) =>
    id + nn.getOrElse("")

private def identifier: ParserS[String] =
  case head &: tail if head(0).isLetter | head.startsWith("__") => (tail, head)

private def int: ParserS[Int] =
  case num &: tail if num.forall(_.isDigit) => (tail, num.toInt)
