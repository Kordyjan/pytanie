package pytanie

def myQuery = query"""
      |query {
      |  repository(name: "dotty", owner: "lampepfl") {
      |    issues(first: 10) {
      |      nodes {
      |        title
      |        labels
      |      }
      |    }
      |  }
      |}"""
