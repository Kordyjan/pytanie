package pytanie

def myQuery = query"""
      |query {
      |  repository(name: "dotty", owner: "lampepfl") {
      |    issues(first: 10, orderBy: {field: CREATED_AT, direction: DESC}) {
      |      nodes {
      |        title
      |        labels(first: 10) {
      |          nodes {
      |           name
      |          }
      |        }
      |      }
      |    }
      |  }
      |}"""

val x = myQuery.get.repository.issues.nodes.flatMap: i =>
      i.labels.nodes

