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
    |query {
    |  organization(login: "lampepfl") {
    |    projectV2(number: 6) {
    |    	items(first: 10) {
    |        pageInfo {
    |          hasNextPage
    |          endCursor
    |        }
    |        nodes {
    |          id
    |          content {
    |            __typename
    |            ... on PullRequest {
    |              number
    |              mergedAt
    |            }
    |          }
    |        }
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
  val x = res.organization.projectV2.items.stream
    .take(30)
    .map(_.content.asPullRequest)
    .collect:
      case Some(pr) => s"#${pr.number.toDouble.toInt}: ${pr.mergedAt}"
    .zipWithIndex
    .map: (str, i) =>
      s"${i + 1}. $str"
    .foreach(println)
