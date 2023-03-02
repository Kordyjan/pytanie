package pytanie.model

case class Query(name: Option[String], selectionSet: SelectionSet)

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
