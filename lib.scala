//> using scala "3.3.0-RC3"
//> using option "--explain"
//> using toolkit "0.1.4"

package pytanie

import quoted.*
import pytanie.parser.parseQuery
import pytanie.model.*
import compiletime.summonInline
import sttp.client3._
import ujson.Str
import ujson.Obj
import ujson.Arr
import ujson.Num
import ujson.False
import ujson.True
import sttp.model.Uri
import upickle.default.Writer

class PreparedQuery[T](text: String, params: ujson.Value):
  def send(url: Uri, username: String, token: String): T =
    val data = ujson.Obj("query" -> text, "variables" -> params)
    val response = basicRequest
      .post(url)
      .auth
      .basic(username, token)
      .body(data.toString)
      .send(HttpClientSyncBackend())
    response.body.match
      case Left(value)  => throw RuntimeException(value)
      case Right(value) => Result(ujson.read(value)("data")).asInstanceOf[T]

class Result(data: ujson.Value) extends Selectable:
  def selectDynamic(name: String): Any =
    data(name) match
      case Str(value) => value
      case Arr(value) => value.map(Result(_)).toList
      case Num(value) => value.toString
      case value      => Result(value)

extension (inline con: StringContext)
  transparent inline def query(inline params: Any*) = ${
    queryImpl('con, 'params)
  }

private def queryImpl(con: Expr[StringContext], paramExprs: Expr[Seq[Any]])(
    using Quotes
): Expr[Any] =
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

  val parts: Seq[String] = con match
    case '{ StringContext($t: _*) } =>
      t match
        case Varargs(parts) => parts.map(_.valueOrAbort)

  val params = paramExprs match
    case Varargs(pars) =>
      pars.zipWithIndex.map: (par, n) =>
        s"par$n" -> par

  val text: String =
    interleave(parts, params.map("$" + _._1)).mkString.stripMargin

  val model = parseQuery(text).get
  val newVariables =
    model.variables.map(_.vars).getOrElse(Nil) ++ params.map(p =>
      VariableDefinition(p._1, "Int!")
    )
  val updatedModel =
    model.copy(variables = Some(VariableDefinitions(newVariables)))

  val paramPairs = params.map: (name, expr) =>
    expr match
      case '{ $value: t } =>
        '{
          ${ Expr(name) } -> upickle.default.writeJs($value)(using
            summonInline[Writer[t]]
          )
        }
  val paramObj = '{ ujson.Obj.from(${ Expr.ofSeq(paramPairs) }) }
  prepareType(model.selectionSet).asType match
    case '[t] =>
      '{ new PreparedQuery[t](${ Expr(updatedModel.sendable) }, $paramObj) }
end queryImpl

private def interleave[A](left: Seq[A], right: Seq[A]): Seq[A] =
  left.map(Seq(_)).zipAll(right.map(Seq(_)), Nil, Nil).flatMap((l, r) => l ++ r)
