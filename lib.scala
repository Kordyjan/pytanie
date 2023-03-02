//> using scala "3.3.0-RC3"
//> using option "--explain"
//> using toolkit "0.1.4"

package pytanie

import quoted.*
import pytanie.parser.parseQuery
import pytanie.model.*

class PreparedQuery[T]:
  def get: T = ???

class Result extends Selectable:
  def selecttDynamic(name: String) = new Result

extension (inline con: StringContext)
  transparent inline def query() = ${ queryImpl('con) }

private def queryImpl(con: Expr[StringContext])(using Quotes): Expr[Any] =
  import quotes.reflect.*

  def prepareType(set: SelectionSet): TypeRepr =
    set.fields.foldLeft(TypeRepr.of[Result]): (acc, f) =>
      val typ =
        f.selectionSet.map(prepareType(_)).getOrElse(TypeRepr.of[Result])
      Refinement(acc, f.name, typ)

  val text = con match
    case '{ StringContext($t) } => t.valueOrAbort.stripMargin

  prepareType(parseQuery(text).get.selectionSet).asType match
    case '[t] => '{ new PreparedQuery[t] }
