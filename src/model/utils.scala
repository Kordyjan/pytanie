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

extension (m: Field | Query)
  private[pytanie] def get(name: String): Field =
    m.match
      case f: Field => f.setFlattened
      case q: Query => q.selectionSet.fields
    .find(_.name == name)
      .get

private[pytanie] def isPaginated(
    set: List[Field],
    arguments: List[Argument]
): Boolean =
  arguments.exists(_.name == "first")
    && set.exists(_.name == "nodes")
    && set.exists: f =>
      f.name == "pageInfo"
        && f.selectionSet.exists(_.fields.exists(_.name == "hasNextPage"))
        && f.selectionSet.exists(_.fields.exists(_.name == "endCursor"))
