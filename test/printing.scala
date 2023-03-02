package pytanie.test

import pytanie.parser.parseQuery

class PrintingSuite extends munit.FunSuite:
  test("simple query"):
    val actual = parseQuery("""
      |query {
      |  viewer {
      |    login
      |  }
      |}""".stripMargin).get.sendable
    assertEquals(actual, "query { viewer { login } }")

  test("query with field with arguments"):
    val actual = parseQuery("""
      |query {
      |  repository(name: "dotty", owner: "lampepfl") {
      |    issues(first: 10) {
      |      nodes {
      |        title
      |      }
      |    }
      |  }
      |}""".stripMargin).get.sendable
    assertEquals(
      actual,
      "query { repository(name: \"dotty\", owner: \"lampepfl\") { issues(first: 10) { nodes { title } } } }"
    )

  test("query with varaibles"):
    val actual = parseQuery("""
      |query issues($first: Int!, $name: String!) {
      |  repository(owner: "lampepfl", name: $name) {
      |    issues(first: $first) {
      |      nodes {
      |        title
      |      }
      |    }
      |  }
      |}""".stripMargin).get.sendable
    assertEquals(
      actual,
      "query issues($first: Int!, $name: String!) { repository(owner: \"lampepfl\", name: $name) { issues(first: $first) { nodes { title } } } }"
    )

  test("query with field with complex arguments"):
    val actual = parseQuery("""
      |query {
      |  repository(name: "dotty", owner: "lampepfl") {
      |    issues(first: 10, filterBy: {states: OPEN}) {
      |      nodes {
      |        title
      |      }
      |    }
      |  }
      |}""".stripMargin).get.sendable
    assertEquals(
      actual,
      "query { repository(name: \"dotty\", owner: \"lampepfl\") { issues(first: 10, filterBy: { states: OPEN }) { nodes { title } } } }"
    )
