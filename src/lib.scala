package pytanie

import quoted.*
import pytanie.parser.parseQuery
import pytanie.model.*
import pytanie.model.utils.*
import compiletime.summonInline
import sttp.client4._
import ujson.Str
import ujson.Obj
import ujson.Arr
import ujson.Num
import sttp.model.Uri
import ToExprModel.given

class PreparedQuery[T](
    private[pytanie] val query: Query,
    private[pytanie] val injectedVars: List[VariableDefinition],
    private[pytanie] val params: ujson.Value
):
  lazy val text = injectedVars match
    case Nil => query.sendable
    case _ =>
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
      case Left(value) => throw RuntimeException(value)
      case Right(value) =>
        val content = ujson.read(value)
        content.obj.get("data") match
          case Some(data) =>
            RootResult(data, query, url, username, token, injectedVars, params)
              .asInstanceOf[T]
          case None => throw RuntimeException(content("errors").toString)

extension (inline con: StringContext)
  transparent inline def query(inline params: Any*) = ${
    queryImpl('con, 'params)
  }

private def queryImpl(con: Expr[StringContext], paramExprs: Expr[Seq[Any]])(
    using Quotes
): Expr[Any] =
  import quotes.reflect.*

  def prepareType(set: List[Selection], arguments: List[Argument]): TypeRepr =
    val typedFields: List[(String, TypeRepr)] =
      set.collect:
        case f: Field =>
          val typ =
            val inner = f.selectionSet match
              case Some(selSet) =>
                prepareType(
                  selSet.selections,
                  f.arguments.toList.flatMap(_.args)
                )
              case None => TypeRepr.of[String]
            if Set("nodes", "edges").contains(f.name) then
              TypeRepr.of[List].appliedTo(inner)
            else inner
          (f.name, typ)

    val unionSelectors: List[(String, TypeRepr)] =
      if isUnion(set) then
        set.collect:
          case f: InlineFragment =>
            (
              s"as${f.conditionType}",
              TypeRepr
                .of[Option]
                .appliedTo(prepareType(f.selectionSet.selections, Nil))
            )
      else Nil

    val seed =
      if isPaginated(set, arguments) then
        typedFields.find(_._1 == "nodes").get._2 match
          case AppliedType(_, List(inner)) =>
            TypeRepr.of[PaginatedResult].appliedTo(inner)
          case _: TypeRepr => TypeRepr.of[Result]
      else TypeRepr.of[Result]

    (typedFields ++ unionSelectors).foldLeft(seed):
      case (acc, (name, typ)) =>
        Refinement(acc, name, typ)
  end prepareType

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
                    Param(
                      name,
                      '{ $v.graphqlType },
                      '{ $v.write($value.asInstanceOf[w]) }
                    )
                  case None =>
                    val tpeName = TypeRepr.of[t].show
                    report.errorAndAbort(
                      s"Variable instance not found for type $tpeName",
                      par
                    )

  val text: String =
    interleave(parts, params.map("$" + _.name)).mkString.stripMargin

  val model = parseQuery(text).get

  val newVariables = params.map: p =>
    '{ VariableDefinition(${ Expr(p.name) }, ${ p.typeName }) }

  val paramPairs = params.map: p =>
    '{ ${ Expr(p.name) } -> ${ p.substitution } }

  val paramObj = '{ ujson.Obj.from(${ Expr.ofSeq(paramPairs) }) }
  prepareType(model.selectionSet.selections, Nil).asType match
    case '[t] =>
      '{
        new PreparedQuery(
          ${ Expr(model) },
          ${ Expr.ofList(newVariables) },
          $paramObj
        ).asInstanceOf[PreparedQuery[t]]
      }
end queryImpl

private def interleave[A](left: Seq[A], right: Seq[A]): Seq[A] =
  left.map(Seq(_)).zipAll(right.map(Seq(_)), Nil, Nil).flatMap((l, r) => l ++ r)

private case class Param(
    name: String,
    typeName: Expr[String],
    substitution: Expr[ujson.Value]
)
