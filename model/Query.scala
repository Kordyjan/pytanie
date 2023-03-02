package pytanie.model

case class Query(
    name: Option[String],
    variables: Option[VariableDefinitions],
    selectionSet: SelectionSet
)

case class VariableDefinitions(vars: List[VariableDefinition])

case class VariableDefinition(name: String, typ: String)

case class SelectionSet(fields: List[Field])

case class Field(
    name: String,
    arguments: Option[Arguments],
    selectionSet: Option[SelectionSet]
)

case class Arguments(args: List[Argument])

case class Argument(name: String, value: Value)

sealed trait Value

case class IntValue(value: Int) extends Value

case class StringValue(value: String) extends Value

case class Variable(name: String) extends Value

case class EnumValue(name: String) extends Value

case class ObjectValue(fields: List[ObjectField]) extends Value

case class ObjectField(name: String, value: Value)
