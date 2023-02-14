//> using lib "org.scalameta::munit:0.7.29"

package pytanie

class SanityTest extends munit.FunSuite:
  test("sanity test"):
    assertEquals(sanity, "It works")
