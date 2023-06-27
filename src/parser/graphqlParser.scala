package pytanie.parser

import pytanie.model.*

private type ParserS[E] = Parser[String, E]

def parseQuery(input: String): Option[Query] =
  query.lift(graphqlTokenizer(input)).map(_._2)

private def query: ParserS[Query] =
  ((just("query") +> identifier.?).? <+> varaibleDefinitions.? <+> selectionSet)
    .map { (name, vars, set) =>
      Query(name.flatten, vars, set)
    }

private def varaibleDefinitions: ParserS[VariableDefinitions] =
  (just("(") +> variableDefinition.separatedBy(just(",")) <+ just(")"))
    .map(VariableDefinitions(_))

private def variableDefinition: ParserS[VariableDefinition] =
  (just("$") +> (identifier <+ just(":")) <+> typ).map(VariableDefinition(_, _))

private def selectionSet: ParserS[SelectionSet] =
  (just("{") +> field.* <+ just("}"))
    .map(SelectionSet(_))

private def field: ParserS[Field] =
  (identifier <+> arguments.? <+> selectionSet.?)
    .map { (name, args, set) =>
      Field(name, args, set)
    }

private def arguments: ParserS[Arguments] =
  (just("(") +> argument.separatedBy(just(",")) <+ just(")")).map(Arguments(_))

private def argument: ParserS[Argument] =
  ((identifier <+ just(":")) <+> value).map { (name, value) =>
    Argument(name, value)
  }

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
  (identifier <+> just("!").?).map { (id, nn) =>
    id + nn.getOrElse("")
  }

private def identifier: ParserS[String] =
  case head &: tail if head(0).isLetter => (tail, head)

private def int: ParserS[Int] =
  case num &: tail if num.forall(_.isDigit) => (tail, num.toInt)
