package pytanie.parser
package test

import munit.FunSuite
import pytanie.test.utils.*

class ParserSuite extends FunSuite:
  given Trace[Any] = NoTrace

  def digitParser: Parser[Char, Char] =
    case d &: tail if d.isDigit => (tail, d)

  def letterParser: Parser[Char, Char] =
    case l &: tail if l.isLetter => (tail, l)

  test("accessing the head works"):
    val head &: tail = charTokenization("abcd"): @unchecked
    assertEquals(head, 'a')

  test("parsers can be composed"):
    val head1 &: head2 &: tail = charTokenization("abcd"): @unchecked
    assertEquals(head1, 'a')
    assertEquals(head2, 'b')

  test("parsers can check equality"):
    val '(' &: char &: ')' &: _ = charTokenization("(x)yz"): @unchecked
    assertEquals(char, 'x')

  test("parsers can check equality and reject"):
    charTokenization("(x]yz") match
      case '(' &: char &: ')' &: _ =>
        fail("parser accepted the input but shuld reject")
      case _ => ()

  test("<+> can concatenate simple parsers"):
    val combined = digitParser <+> letterParser
    combined
      .lift(charTokenization("4f!"))
      .assertMatch:
        case Some((Token('!', _, _) #:: _, ('4', 'f'))) =>

  test("<+> can concatenate more complex parsers"):
    val combined = digitParser <+> letterParser
    val complex = combined <+> combined
    complex
      .lift(charTokenization("4f3e"))
      .assertMatch:
        case Some((LazyList(), ('4', 'f', '3', 'e'))) =>

  test("<+> can fail on first term"):
    val parser = digitParser <+> letterParser
    parser
      .lift(charTokenization("abc"))
      .assertMatch:
        case None =>

  test("<+> can fail on second term"):
    val parser = digitParser <+> letterParser
    parser
      .lift(charTokenization("123"))
      .assertMatch:
        case None =>

  test("<+> can fail later on longer chains"):
    val parser =
      digitParser <+> letterParser <+> digitParser <+> letterParser <+> digitParser
    parser
      .lift(charTokenization("1a2bc"))
      .assertMatch:
        case None =>

  test("just can capture a token"):
    just('a')
      .lift(charTokenization("abc"))
      .assertMatch:
        case Some((_, 'a')) =>

  test("just can reject a token"):
    just('a')
      .lift(charTokenization("xyz"))
      .assertMatch:
        case None =>

  test("map works"):
    val parser = letterParser.map(_.toUpper)
    parser
      .lift(charTokenization("abcd"))
      .assertMatch:
        case Some((_, 'A')) =>

  test("map doesn't change the parsing failure"):
    val parser = letterParser.map(_ => 'x')
    parser
      .lift(charTokenization("0abc"))
      .assertMatch:
        case None =>
