package pytanie.model

case class Query(
    kind: Kind,
    name: Option[String],
    variables: Option[VariableDefinitions],
    selectionSet: SelectionSet
):
  def sendable: String =
    val kindS = kind match
      case Kind.Mutation => "mutation"
      case Kind.Query    => "query"
    val nameS = name.map(" " + _).getOrElse("")
    val varsS = variables.map(_.sendable).getOrElse("")
    s"$kindS$nameS$varsS ${selectionSet.sendable}"

enum Kind:
  case Mutation, Query

case class VariableDefinitions(vars: List[VariableDefinition]):
  def sendable = vars.map(_.sendable).mkString("(", ", ", ")")

case class VariableDefinition(name: String, typ: String):
  def sendable = "$" + s"$name: $typ"

case class SelectionSet(selections: List[Selection]):
  def sendable = selections.map(_.sendable).mkString("{ ", " ", " }")

sealed trait Selection:
  def sendable: String

case class InlineFragment(conditionType: String, selectionSet: SelectionSet)
    extends Selection:
  def sendable = s"... on $conditionType ${selectionSet.sendable}"

case class Field(
    alias: Option[String],
    name: String,
    arguments: Option[Arguments],
    selectionSet: Option[SelectionSet]
) extends Selection:
  lazy val effectiveName = alias.getOrElse(name)

  def sendable: String =
    val al = alias.map(_ + ": ").getOrElse("")
    val args = arguments.map(_.sendable).getOrElse("")
    val sels = selectionSet.map(s => s" ${s.sendable}").getOrElse("")
    s"$al$name$args$sels"

case class Arguments(args: List[Argument]):
  def sendable = args.map(_.sendable).mkString("(", ", ", ")")

case class Argument(name: String, value: Value):
  def sendable = s"$name: ${value.sendable}"

sealed trait Value:
  def sendable: String

case class IntValue(value: Int) extends Value:
  def sendable = value.toString

case class StringValue(value: String) extends Value:
  def sendable = s""""$value""""

case class Variable(name: String) extends Value:
  def sendable = "$" + name

case class EnumValue(name: String) extends Value:
  def sendable = name

case class ObjectValue(fields: List[ObjectField]) extends Value:
  override def sendable: String =
    if fields.isEmpty then "{}"
    else
      val fielsS = fields.map(_.sendable).mkString(", ")
      s"{ $fielsS }"

case class ListValue(values: List[Value]) extends Value:
  override def sendable: String =
    if values.isEmpty then "[]"
    else
      val valuesS = values.map(_.sendable).mkString(", ")
      s"[ $valuesS ]"

case class ObjectField(name: String, value: Value):
  def sendable = s"$name: ${value.sendable}"
