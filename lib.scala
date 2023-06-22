//> using scala 3.3.0

//> using option -Wunused:all -Wvalue-discard
//> using toolkit latest
//> using test.dep org.scala-lang::toolkit-test:0.1.7

package pytanie

import quoted.*
import pytanie.parser.parseQuery
import pytanie.model.*
import compiletime.summonInline
import sttp.client4._
import ujson.Str
import ujson.Obj
import ujson.Arr
import ujson.Num
import sttp.model.Uri
import ToExprModel.given

class PreparedQuery[T](query: Query, injectedVars: List[VariableDefinition], params: ujson.Value):
  lazy val text = injectedVars match
    case Nil => query.sendable
    case _   =>
      val oldVars = query.variables.map(_.vars).getOrElse(Nil)
      val newVars = VariableDefinitions(oldVars ++ injectedVars)
      query.copy(variables = Some(newVars)).sendable

  def send(url: Uri, username: String, token: String): T =
    val data = ujson.Obj("query" -> text, "variables" -> params)
    val response = basicRequest
      .post(url)
      .auth
      .basic(username, token)
      .body(data.toString)
      .send(DefaultSyncBackend())
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
        val name = s"par$n"
        par match
          case '{ $value: t } =>
            TypeRepr.of[t].widenTermRefByName.asType match
              case '[w] =>
                Expr.summon[pytanie.Variable[w]] match
                  case Some(v) =>
                    Param(name, '{ $v.graphqlType}, '{ $v.write($value.asInstanceOf[w]) })
                  case None =>
                    val tpeName = TypeRepr.of[t].show
                    report.errorAndAbort(s"Variable instance not found for type $tpeName", par)

  val text: String =
    interleave(parts, params.map("$" + _.name)).mkString.stripMargin

  val model = parseQuery(text).get

  val newVariables = params.map: p =>
      '{ VariableDefinition(${Expr(p.name)}, ${p.typeName}) }

  val paramPairs = params.map: p =>
    '{ ${Expr(p.name)} -> ${p.substitution} }

  val paramObj = '{ ujson.Obj.from(${ Expr.ofSeq(paramPairs) }) }
  prepareType(model.selectionSet).asType match
    case '[t] =>
      '{ new PreparedQuery(${Expr(model)}, ${Expr.ofList(newVariables)}, $paramObj).asInstanceOf[PreparedQuery[t]] }
end queryImpl

private def interleave[A](left: Seq[A], right: Seq[A]): Seq[A] =
  left.map(Seq(_)).zipAll(right.map(Seq(_)), Nil, Nil).flatMap((l, r) => l ++ r)

private case class Param(name: String, typeName: Expr[String], substitution: Expr[ujson.Value])