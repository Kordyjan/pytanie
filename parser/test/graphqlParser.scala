package pytanie.parser
package test

import pytanie.model.*
import pytanie.test.utils.*

class GraphqlParserSuite extends munit.FunSuite:
  test("simple query"):
    parseQuery("""
      |query {
      |  viewer {
      |    login
      |  }
      |}""".stripMargin).assertMatch:
      case Some(
            Query(
              None,
              SelectionSet(
                List(
                  Field("viewer", Some(SelectionSet(List(Field(login, None)))))
                )
              )
            )
          ) =>
