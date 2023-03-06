//> using scala "3.3.0-RC3"
//> using option "--explain"
//> using toolkit "0.1.4"
//> using dep "com.lihaoyi::requests:0.8.0"

package pytanie

import quoted.*
import pytanie.parser.parseQuery
import pytanie.model.*
import requests.RequestAuth
import ujson.Str
import ujson.Obj
import ujson.Arr
import ujson.Num
import ujson.False
import ujson.True

class PreparedQuery[T](text: String):
  def send(url: String, auth: RequestAuth): T =
    val data = ujson.Obj("query" -> text)
    val response = requests.post(url, auth = auth, data = data.toString).text()
    Result(ujson.read(response)("data")).asInstanceOf[T]

class Result(data: ujson.Value) extends Selectable:
  def selectDynamic(name: String): Any =
    data(name) match
      case Str(value) => value
      case Arr(value) => value.map(Result(_)).toList
      case value      => Result(value)

extension (inline con: StringContext)
  transparent inline def query() = ${ queryImpl('con) }

private def queryImpl(con: Expr[StringContext])(using Quotes): Expr[Any] =
  import quotes.reflect.*

  def prepareType(set: SelectionSet): TypeRepr =
    set.fields.foldLeft(TypeRepr.of[Result]): (acc, f) =>
      val typ =
        val inner = f.selectionSet match
          case Some(selSet) => prepareType(selSet)
          case None         => TypeRepr.of[String]
        if Set("nodes", "edges").contains(f.name) then
          TypeRepr.of[List].appliedTo(inner)
        else inner
      Refinement(acc, f.name, typ)

  val text = con match
    case '{ StringContext($t) } => t.valueOrAbort.stripMargin

  prepareType(parseQuery(text).get.selectionSet).asType match
    case '[t] => '{ new PreparedQuery[t](${ Expr(text) }) }
end queryImpl
