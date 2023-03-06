package pytanie

import java.nio.file.{Files, Paths}
import sttp.client3._

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
  val res = myQuery.send(
    uri"https://api.github.com/graphql",
    "Kordyjan",
    os.read(os.pwd / os.up / "keyfile.txt")
  )
  val x = res.repository.issues.nodes.map(_.number)

  println(x)
