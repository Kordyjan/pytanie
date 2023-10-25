package pytanie.model
package utils

import pytanie.PreparedQuery
import pytanie.Result

extension (f: Field)

  private[pytanie] def argumentsFlattened: List[Argument] =
    f.arguments.toList.flatMap(_.args)

  private[pytanie] def setFlattened: List[Field] =
    f.selectionSet.toList.flatMap(_.fields)

  private[pytanie] def withArgument(name: String, value: String): Field =
    val arguments = f.argumentsFlattened
    if arguments.exists(_.name == name) then
      f.copy(arguments = Some(Arguments(arguments.map:
        case arg if arg.name == name => arg.copy(value = StringValue(value))
        case arg                     => arg
      )))
    else
      f.copy(arguments =
        Some(Arguments(arguments :+ Argument(name, StringValue(value))))
      )

extension (m: Selection | Query)
  private[pytanie] def getField(name: String): Field =
    m.match
      case f: Field => f.setFlattened
      case f: InlineFragment => f.selectionSet.fields
      case q: Query => q.selectionSet.fields
    .find(_.name == name)
    .get

  private[pytanie] def getFragment(name: String): InlineFragment =
    m.match
      case q: Query => q.selectionSet.selections
      case f: Field => f.selectionSet.toList.flatMap(_.selections)
      case f: InlineFragment => f.selectionSet.selections
    .collectFirst:
      case frag: InlineFragment if frag.conditionType == name => frag
    .get


private[pytanie] def isPaginated(
    set: List[Selection],
    arguments: List[Argument]
): Boolean =
  val fields = set.collect:
    case f: Field => f

  arguments.exists(_.name == "first")
    && fields.exists(_.name == "nodes")
    && fields.exists: f =>
      f.name == "pageInfo"
        && f.selectionSet.exists(_.fields.exists(_.name == "hasNextPage"))
        && f.selectionSet.exists(_.fields.exists(_.name == "endCursor"))

private[pytanie] def isUnion(set: List[Selection]): Boolean =
  val hasTypename = set
    .collectFirst:
      case f: Field if f.name == "__typename" => true
    .getOrElse(false)
  val hasFragments = set
    .collectFirst:
      case f: InlineFragment => true
    .getOrElse(false)
  hasTypename && hasFragments

extension (set: SelectionSet)
  def fields = set.selections.collect:
    case f: Field => f

extension (s: Selection)
  def label = s match
    case f: Field => f.name
    case f: InlineFragment => f.conditionType

