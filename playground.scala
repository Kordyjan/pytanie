package pytanie

import requests.RequestAuth
import java.nio.file.{Files, Paths}

@main def playground =
  val myQuery = query"""
    |{
    |  repository(name: "dotty", owner: "lampepfl") {
    |    issues(
    |      first: 15,
    |      filterBy: {states: OPEN},
    |      orderBy: {field: CREATED_AT, direction: DESC}
    |    ) {
    |      nodes {
    |        number
    |        title
    |        labels(first: 10) {
    |          nodes {
    |            name
    |          }
    |        }
    |      }
    |    }
    |  }
    |}
  """
  val auth = RequestAuth.Basic(
    "Kordyjan",
    Files.readAllLines(Paths.get("../keyfile.txt")).get(0)
  )
  val res = myQuery.send("https://api.github.com/graphql", auth)

  val x = res.repository.issues.nodes
    .map: i =>
      i.number
    .toSet

  println(x)
