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
              Kind.Query,
              None,
              None,
              SelectionSet(
                List(
                  Field(
                    None,
                    "viewer",
                    None,
                    Some(SelectionSet(List(Field(None, login, None, None))))
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
              Kind.Query,
              None,
              None,
              SelectionSet(
                List(
                  Field(
                    None,
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
                            None,
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
                                    None,
                                    "nodes",
                                    None,
                                    Some(
                                      SelectionSet(
                                        List(
                                          Field(None, title, None, None)
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

  test("query with varaibles"):
    parseQuery("""
      |query issues($first: Int!, $name: String!) {
      |  repository(owner: "lampepfl", name: $name) {
      |    issues(first: $first) {
      |      nodes {
      |        title
      |      }
      |    }
      |  }
      |}""".stripMargin).assertMatch:
      case Some(
            Query(
              Kind.Query,
              Some("issues"),
              Some(
                VariableDefinitions(
                  List(
                    VariableDefinition("first", "Int!"),
                    VariableDefinition("name", "String!")
                  )
                )
              ),
              SelectionSet(
                List(
                  Field(
                    None,
                    "repository",
                    Some(
                      Arguments(
                        List(
                          Argument("owner", StringValue("lampepfl")),
                          Argument("name", Variable("name"))
                        )
                      )
                    ),
                    Some(
                      SelectionSet(
                        List(
                          Field(
                            None,
                            "issues",
                            Some(
                              Arguments(
                                List(
                                  Argument("first", Variable("first"))
                                )
                              )
                            ),
                            Some(
                              SelectionSet(
                                List(
                                  Field(
                                    None,
                                    "nodes",
                                    None,
                                    Some(
                                      SelectionSet(
                                        List(
                                          Field(None, title, None, None)
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

  test("query with field with complex arguments"):
    parseQuery("""
      |query {
      |  repository(name: "dotty", owner: "lampepfl") {
      |    issues(first: 10, filterBy: {states: OPEN}) {
      |      nodes {
      |        title
      |      }
      |    }
      |  }
      |}""".stripMargin).assertMatch:
      case Some(
            Query(
              Kind.Query,
              None,
              None,
              SelectionSet(
                List(
                  Field(
                    None,
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
                            None,
                            "issues",
                            Some(
                              Arguments(
                                List(
                                  Argument("first", IntValue(10)),
                                  Argument(
                                    "filterBy",
                                    ObjectValue(
                                      List(
                                        ObjectField(
                                          "states",
                                          EnumValue("OPEN")
                                        )
                                      )
                                    )
                                  )
                                )
                              )
                            ),
                            Some(_)
                          )
                        )
                      )
                    )
                  )
                )
              )
            )
          ) =>

  test("mutation"):
    parseQuery("""
    |mutation {
    |  updatePullRequest(
    |    input: {pullRequestId: "PR_kwDOJ84k7M5Vx8Ao", milestoneId: "MI_kwDOJ84k7M4Ak4ai"}
    |  ) {
    |    clientMutationId
    |  }
    |}
    """.stripMargin).assertMatch:
      case Some(
            Query(
              Kind.Mutation,
              None,
              None,
              SelectionSet(
                List(
                  Field(
                    None,
                    "updatePullRequest",
                    Some(
                      Arguments(
                        List(
                          Argument(
                            "input",
                            ObjectValue(
                              List(
                                ObjectField(
                                  "pullRequestId",
                                  StringValue("PR_kwDOJ84k7M5Vx8Ao")
                                ),
                                ObjectField(
                                  "milestoneId",
                                  StringValue("MI_kwDOJ84k7M4Ak4ai")
                                )
                              )
                            )
                          )
                        )
                      )
                    ),
                    Some(
                      SelectionSet(
                        List(
                          Field(
                            None,
                            "clientMutationId",
                            None,
                            None
                          )
                        )
                      )
                    )
                  )
                )
              )
            )
          ) =>

  test("inline fragment"):
    parseQuery("""
    |query {
    |  organization(login: "lampepfl") {
    |    projectV2(number: 6) {
    |      items(first: 10) {
    |        nodes {
    |          content {
    |            ... on PullRequest {
    |              mergedAt
    |            }
    |          }
    |        }
    |      }
    |    }
    |  }
    |}
    """.stripMargin).assertMatch:
      case Some(
            Query(
              Kind.Query,
              None,
              None,
              SelectionSet(
                List(
                  Field(
                    None,
                    "organization",
                    Some(
                      Arguments(
                        List(
                          Argument("login", StringValue("lampepfl"))
                        )
                      )
                    ),
                    Some(
                      SelectionSet(
                        List(
                          Field(
                            None,
                            "projectV2",
                            Some(
                              Arguments(
                                List(
                                  Argument("number", IntValue(6))
                                )
                              )
                            ),
                            Some(
                              SelectionSet(
                                List(
                                  Field(
                                    None,
                                    "items",
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
                                            None,
                                            "nodes",
                                            None,
                                            Some(
                                              SelectionSet(
                                                List(
                                                  Field(
                                                    None,
                                                    "content",
                                                    None,
                                                    Some(
                                                      SelectionSet(
                                                        List(
                                                          InlineFragment(
                                                            "PullRequest",
                                                            SelectionSet(
                                                              List(
                                                                Field(
                                                                  None,
                                                                  "mergedAt",
                                                                  None,
                                                                  None
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

  test("list value"):
    parseQuery("""
    |query {
    |  repository(name: "dotty", owner: "lampepfl") {
    |    issues(first: 10, filterBy: {states: [OPEN, CLOSED]}) {
    |      nodes {
    |        title
    |      }
    |    }
    |  }
    |}""".stripMargin).assertMatch:
      case Some(
            Query(
              Kind.Query,
              None,
              None,
              SelectionSet(
                List(
                  Field(
                    None,
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
                            None,
                            "issues",
                            Some(
                              Arguments(
                                List(
                                  Argument("first", IntValue(10)),
                                  Argument(
                                    "filterBy",
                                    ObjectValue(
                                      List(
                                        ObjectField(
                                          "states",
                                          ListValue(
                                            List(
                                              EnumValue("OPEN"),
                                              EnumValue("CLOSED")
                                            )
                                          )
                                        )
                                      )
                                    )
                                  )
                                )
                              )
                            ),
                            Some(_)
                          )
                        )
                      )
                    )
                  )
                )
              )
            )
      ) =>

  test("query with aliases"):
      parseQuery("""
      |query {
      |  v: viewer {
      |    l: login
      |  }
      |}""".stripMargin).assertMatch:
        case Some(
              Query(
                Kind.Query,
                None,
                None,
                SelectionSet(
                  List(
                    Field(
                      Some("v"),
                      "viewer",
                      None,
                      Some(
                        SelectionSet(
                          List(
                            Field(Some("l"), "login", None, None)
                          )
                        )
                      )
                    )
                  )
                )
              )
            ) =>

