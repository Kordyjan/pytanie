//> using files src debugging.scala
//> using scala 3.3.0
//> using toolkit latest
//> using options -Xcheck-macros

package pytanie

import sttp.client4._
import ujson.Value
import debugging.*

@main def playground =
  val number = 10
  val repoName = "dotty"

  val myQuery = query"""
    |{
    |  repository(name: ${repoName}, owner: "lampepfl") {
    |    issues(
    |      first: ${number},
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
    |      pageInfo {
    |        hasNextPage
    |        endCursor
    |      }
    |    }
    |  }
    |}
  """
  // println(typeOf(myQuery))

  val res = myQuery.send(
    uri"https://api.github.com/graphql",
    "Kordyjan",
    os.read(os.pwd / os.up / "key.txt")
  )
  val x = res.repository.issues.stream.map(_.title).take(30)

  x.foreach(println)
