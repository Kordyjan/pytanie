package pytanie.model

import scala.quoted.*

object ToExprModel:
  given ToExpr[Query] with
    def apply(q: Query)(using Quotes) =
      '{
        Query(
          ${ Expr(q.kind) },
          ${ Expr(q.name) },
          ${ Expr(q.variables) },
          ${ Expr(q.selectionSet) }
        )
      }

  given ToExpr[Kind] with
    def apply(k: Kind)(using Quotes) = k match
      case Kind.Mutation => '{ Kind.Mutation }
      case Kind.Query    => '{ Kind.Query }

  given ToExpr[VariableDefinitions] with
    def apply(v: VariableDefinitions)(using Quotes) =
      '{ VariableDefinitions(${ Expr(v.vars) }) }

  given ToExpr[VariableDefinition] with
    def apply(v: VariableDefinition)(using Quotes) =
      '{ VariableDefinition(${ Expr(v.name) }, ${ Expr(v.typ) }) }

  given ToExpr[SelectionSet] with
    def apply(s: SelectionSet)(using Quotes) =
      '{ SelectionSet(${ Expr(s.selections) }) }

  given ToExpr[Selection] with
    def apply(s: Selection)(using Quotes) = s match
      case f: InlineFragment => summon[ToExpr[InlineFragment]](f)
      case f: Field => summon[ToExpr[Field]](f)


  given ToExpr[InlineFragment] with
    def apply(f: InlineFragment)(using Quotes) =
      '{
        InlineFragment(
          ${ Expr(f.conditionType) },
          ${ Expr(f.selectionSet) }
        )
      }

  given ToExpr[Field] with
    def apply(f: Field)(using Quotes) =
      '{
        Field(
          ${ Expr(f.name) },
          ${ Expr(f.arguments) },
          ${ Expr(f.selectionSet) }
        )
      }

  given ToExpr[Arguments] with
    def apply(a: Arguments)(using Quotes) =
      '{ Arguments(${ Expr(a.args) }) }

  given ToExpr[Argument] with
    def apply(a: Argument)(using Quotes) =
      '{ Argument(${ Expr(a.name) }, ${ Expr(a.value) }) }

  given ToExpr[Value] with
    def apply(v: Value)(using Quotes): Expr[Value] = v match
      case IntValue(value)    => '{ IntValue(${ Expr(value) }) }
      case StringValue(value) => '{ StringValue(${ Expr(value) }) }
      case Variable(name)     => '{ Variable(${ Expr(name) }) }
      case EnumValue(name)    => '{ EnumValue(${ Expr(name) }) }
      case ObjectValue(fields) =>
        '{ ObjectValue(${ Expr.ofList(fields.map(Expr(_))) }) }

  given ToExpr[ObjectField] with
    def apply(o: ObjectField)(using Quotes) =
      '{ ObjectField(${ Expr(o.name) }, ${ Expr(o.value) }) }
