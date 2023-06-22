package pytanie.model

import scala.quoted.*

object ToExprModel:
  given ToExpr[Query] with
    def apply(q: Query)(using Quotes) =
      '{Query(${Expr(q.name)}, ${Expr(q.variables)}, ${Expr(q.selectionSet)})}

  given ToExpr[VariableDefinitions] with
    def apply(v: VariableDefinitions)(using Quotes) =
      '{VariableDefinitions(${Expr(v.vars)})}

  given ToExpr[VariableDefinition] with
    def apply(v: VariableDefinition)(using Quotes) =
      '{VariableDefinition(${Expr(v.name)}, ${Expr(v.typ)})}

  given ToExpr[SelectionSet] with
    def apply(s: SelectionSet)(using Quotes) =
      '{SelectionSet(${Expr(s.fields)})}

  given ToExpr[Field] with
    def apply(f: Field)(using Quotes) =
      '{Field(${Expr(f.name)}, ${Expr(f.arguments)}, ${Expr(f.selectionSet)})}

  given ToExpr[Arguments] with
    def apply(a: Arguments)(using Quotes) =
      '{Arguments(${Expr(a.args)})}

  given ToExpr[Argument] with
    def apply(a: Argument)(using Quotes) =
      '{Argument(${Expr(a.name)}, ${Expr(a.value)})}

  given ToExpr[Value] with
    def apply(v: Value)(using Quotes): Expr[Value] = v match
      case IntValue(value) => '{IntValue(${Expr(value)})}
      case StringValue(value) => '{StringValue(${Expr(value)})}
      case Variable(name) => '{Variable(${Expr(name)})}
      case EnumValue(name) => '{EnumValue(${Expr(name)})}
      case ObjectValue(fields) => '{ObjectValue(${Expr.ofList(fields.map(Expr(_)))})}

  given ToExpr[ObjectField] with
    def apply(o: ObjectField)(using Quotes) =
      '{ObjectField(${Expr(o.name)}, ${Expr(o.value)})}
