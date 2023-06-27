//> using files src
//> using scala 3.2.2
//> using toolkit latest


package pytanie

import sttp.client4._
import ujson.Value

@main def playground =
  val number = 15
  val repoName = "dotty"

  val myQuery = query"""
    |{
    |  repository(name: ${ repoName }, owner: "lampepfl") {
    |    issues(
    |      first: ${number + 5},
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
    os.read(os.pwd / os.up / "key.txt")
  )
  val x = res.repository.issues.nodes.flatMap(_.labels.nodes).map(_.name).toSet

  x.foreach(println)
