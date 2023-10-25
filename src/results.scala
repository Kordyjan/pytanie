package pytanie

import model.*
import model.utils.*
import ujson.*
import sttp.model.Uri

trait Result extends Selectable:
  private[pytanie] def data: ujson.Value
  private[pytanie] def model: Selection | Query
  private[pytanie] def parent: Option[Result]

  private[pytanie] def resendPatched(name: String, field: Selection): Result

  def selectDynamic(name: String): Any =
    val fragment = """as(\w+)""".r
    name match
      case fragment(typ) =>
        if data("__typename").str == typ then
          val fragment = model.getFragment(typ)
          Some(FragmentResult(data, fragment, this))
        else None
      case _ =>
        val field = model.getField(name)
        data(name) match
          case Str(value) => value
          case Arr(value) => value.map(FieldResult(_, field, this)).toList
          case Num(value) => value.toString
          case value
              if isPaginated(field.setFlattened, field.argumentsFlattened) =>
            PaginatedResult(value, field, this)
          case value => FieldResult(value, field, this)
end Result

class FieldResult(
    private[pytanie] val data: ujson.Value,
    private[pytanie] val model: Field,
    parentF: Result
) extends Result:
  private[pytanie] def parent = Some(parentF)

  private[pytanie] def resendPatched(name: String, field: Selection): Result =
    val selfName = model.name
    val newFields = model.selectionSet.get.selections.map:
      case f if f.label == name => field
      case f                    => f
    val newSelf = model.copy(selectionSet = Some(SelectionSet(newFields)))
    parentF
      .resendPatched(selfName, newSelf)
      .selectDynamic(selfName)
      .asInstanceOf[Result]

class RootResult(
    private[pytanie] val data: ujson.Value,
    private[pytanie] val model: Query,
    url: Uri,
    username: String,
    token: String,
    injectedVars: List[VariableDefinition],
    params: ujson.Value
) extends Result:
  private[pytanie] def parent = None

  private[pytanie] def resendPatched(name: String, field: Selection): Result =
    val newFields = model.selectionSet.selections.map:
      case f if f.label == name => field
      case f                    => f
    val newModel = model.copy(selectionSet = SelectionSet(newFields))
    PreparedQuery[Result](newModel, injectedVars, params)
      .send(url, username, token)
      .asInstanceOf[Result]

class FragmentResult(
    private[pytanie] val data: ujson.Value,
    private[pytanie] val model: InlineFragment,
    parentF: Result
) extends Result:
  private[pytanie] def parent = Some(parentF)

  private[pytanie] def resendPatched(name: String, field: Selection): Result =
    val selfName = model.conditionType
    val newFields = model.selectionSet.fields.map:
      case f if f.label == name => field
      case f                    => f
    val newSelf = model.copy(selectionSet = SelectionSet(newFields))
    parentF
      .resendPatched(selfName, newSelf)
      .selectDynamic(selfName)
      .asInstanceOf[Result]

class PaginatedResult[T](
    private[pytanie] val data: ujson.Value,
    private[pytanie] val model: Field,
    private[pytanie] val parentF: Result
) extends Result:
  private[pytanie] def parent = Some(parentF)

  def stream: LazyList[T] =
    val lazyList: LazyList[ujson.Value] = LazyList.unfold((this, 0)):
      (page, n) =>
        page.data("nodes") match
          case Arr(nodes) =>
            if (nodes.length > n)
            then Some((nodes(n), (page, n + 1)))
            else if page.data("pageInfo")("hasNextPage").bool then
              val nextPage = page.nextPage
              Some((page.nextPage.data("nodes")(0), (page.nextPage, 1)))
            else None
          case _ =>
            throw IllegalStateException(s"${model.name} is not paginated")
    val elemModel = model.getField("nodes")
    lazyList.map(FieldResult(_, elemModel, this).asInstanceOf[T])

  // temporary implementation copied from NormalResult
  // TODO: make it work for nested pages
  private[pytanie] def resendPatched(name: String, field: Selection): Result =
    val selfName = model.name
    val newFields = model.selectionSet.get.fields.map:
      case f if f.label == name => field
      case f                    => f
    val newSelf = model.copy(selectionSet = Some(SelectionSet(newFields)))
    parentF
      .resendPatched(selfName, newSelf)
      .selectDynamic(selfName)
      .asInstanceOf[Result]

object PaginatedResult:
  extension [T <: PaginatedResult[?]](p: T)
    def nextPage: T =
      val name = p.model.name
      val endCursor = p.data("pageInfo")("endCursor").str
      val newModel = p.model.withArgument("after", endCursor)
      p.parentF
        .resendPatched(name, newModel)
        .selectDynamic(name)
        .asInstanceOf[T]
