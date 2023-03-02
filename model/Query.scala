package pytanie.model

case class Query(name: Option[String], selectionSet: SelectionSet)

case class SelectionSet(fields: List[Field])

case class Field(name: String, selectionSet: Option[SelectionSet])
