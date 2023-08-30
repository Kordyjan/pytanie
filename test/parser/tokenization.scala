package pytanie.parser
package test

import munit.FunSuite
import pytanie.test.utils.*

def charTokenization(using Trace[Char]): String => Tokens[Char] =
  tokenize: (input, pos) =>
    if pos < input.length then
      log(input(pos))
      Some(Token(input(pos), pos, pos + 1))
    else None

class TokenizationSuite extends FunSuite:
  given Trace[Any] = NoTrace

  test("empty string gives no token"):
    assertEquals(charTokenization("").toList, Nil)

  test("tokenization works"):
    val data = "abcdABCD"
    val expected = List(
      Token('a', 0, 1),
      Token('b', 1, 2),
      Token('c', 2, 3),
      Token('d', 3, 4),
      Token('A', 4, 5),
      Token('B', 5, 6),
      Token('C', 6, 7),
      Token('D', 7, 8)
    )
    assertEquals(charTokenization(data).toList, expected)

  test("tokenization is lazy"):
    given LogingTrace[Char] = new LogingTrace

    val data = "abcdABCD"
    val expectedLog = "abcdA"
    val expectedFront1 = List(
      Token('a', 0, 1),
      Token('b', 1, 2),
      Token('c', 2, 3)
    )
    val expectedFront2 = List(
      Token('a', 0, 1),
      Token('b', 1, 2),
      Token('c', 2, 3),
      Token('d', 3, 4),
      Token('A', 4, 5)
    )

    val tokenization = charTokenization(data)
    assertEquals(summon[LogingTrace[Char]].content.mkString, "")

    val front1 = tokenization.take(3).toList
    assertEquals(front1, expectedFront1)

    val front2 = tokenization.take(5).toList
    assertEquals(front2, expectedFront2)
    assertEquals(summon[LogingTrace[Char]].content.mkString, expectedLog)
