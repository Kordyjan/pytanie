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
                  Field(
                    "viewer",
                    None,
                    Some(SelectionSet(List(Field(login, None, None))))
                  )
                )
              )
            )
          ) =>

  test("query with field with arguments"):
    parseQuery("""
      |query {
      |  repository(name: "dotty", owner: "lampepfl") {
      |    issues(first: 10) {
      |      nodes {
      |        title
      |      }
      |    }
      |  }
      |}""".stripMargin).assertMatch:
      case Some(
            Query(
              None,
              SelectionSet(
                List(
                  Field(
                    "repository",
                    Some(
                      Arguments(
                        List(
                          Argument("name", StringValue("dotty")),
                          Argument("owner", StringValue("lampepfl"))
                        )
                      )
                    ),
                    Some(
                      SelectionSet(
                        List(
                          Field(
                            "issues",
                            Some(
                              Arguments(
                                List(
                                  Argument("first", IntValue(10))
                                )
                              )
                            ),
                            Some(
                              SelectionSet(
                                List(
                                  Field(
                                    "nodes",
                                    None,
                                    Some(
                                      SelectionSet(
                                        List(
                                          Field(title, None, None)
                                        )
                                      )
                                    )
                                  )
                                )
                              )
                            )
                          )
                        )
                      )
                    )
                  )
                )
              )
            )
          ) =>
