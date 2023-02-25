package pytanie.parser
package test

import munit.FunSuite
import pytanie.test.utils.*
import scala.compiletime.ops.int

class ParserSuite extends FunSuite:
  given Trace[Any] = NoTrace

  def digitParser: Parser[Char, Char] =
    case d &: tail if d.isDigit => (tail, d)

  def letterParser: Parser[Char, Char] =
    case l &: tail if l.isLetter => (tail, l)

  def intDigitParser: Parser[Char, Int] =
    case d &: tail if d.isDigit => (tail, d - 0x30)

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

  test("+> correctly accepts values"):
      val parser = letterParser +> digitParser
      parser
        .lift(charTokenization("a1b2"))
        .assertMatch:
          case Some((_, '1')) =>

  test("+> can reject on first element"):
      val parser = letterParser +> digitParser
      parser
        .lift(charTokenization("01b2"))
        .assertMatch:
          case None =>

  test("+> can reject on second element"):
      val parser = letterParser +> digitParser
      parser
        .lift(charTokenization("axb2"))
        .assertMatch:
          case None =>

  test("+> can be chained"):
      val parser = digitParser +> digitParser +> digitParser +> letterParser
      parser
        .lift(charTokenization("012a"))
        .assertMatch:
          case Some((_, 'a')) =>

  test("+> can have long chain on right side"):
      val parser = (digitParser <+> digitParser <+> digitParser) +> letterParser
      parser
        .lift(charTokenization("012a"))
        .assertMatch:
          case Some((_, 'a')) =>

  test("<+ correctly accepts values"):
      val parser = letterParser <+ digitParser
      parser
        .lift(charTokenization("a1b2"))
        .assertMatch:
          case Some((_, 'a')) =>

  test("<+ can reject on first element"):
      val parser = letterParser <+ digitParser
      parser
        .lift(charTokenization("01b2"))
        .assertMatch:
          case None =>

  test("<+ can reject on second element"):
      val parser = letterParser <+ digitParser
      parser
        .lift(charTokenization("axb2"))
        .assertMatch:
          case None =>

  test("<+ can be chained"):
      val parser = letterParser <+ digitParser <+ digitParser <+ digitParser
      parser
        .lift(charTokenization("a012"))
        .assertMatch:
          case Some((_, 'a')) =>

  test("<+ can have long chain on left side"):
      val parser = letterParser <+ (digitParser <+> digitParser <+> digitParser)
      parser
        .lift(charTokenization("a012"))
        .assertMatch:
          case Some((_, 'a')) =>

  test("alternative can select left"):
      val parser = letterParser <|> digitParser
      parser
        .lift(charTokenization("a0"))
        .assertMatch:
          case Some((_, 'a')) =>

  test("alternative can select right"):
      val parser = letterParser <|> digitParser
      parser
        .lift(charTokenization("0a"))
        .assertMatch:
          case Some((_, '0')) =>

  test("alternative prefers left"):
      val parser = intDigitParser <|> digitParser
      parser
        .lift(charTokenization("8x"))
        .assertMatch:
          case Some((_, 8)) =>

  test("alternative can fail"):
      val parser = intDigitParser <|> digitParser
      parser
        .lift(charTokenization("xyz"))
        .assertMatch:
          case None =>

  test("alternative can be chained"):
      val parser = intDigitParser <|> digitParser <|> just('*') <|> just(
        '^'
      ) <|> letterParser
      parser
        .lift(charTokenization("^^"))
        .assertMatch:
          case Some((_, '^')) =>

  test("alternative can backtrack"):
      val parserA = just('*') +> letterParser +> digitParser +> letterParser
      val parserB = just('*') +> letterParser +> digitParser +> intDigitParser
      val parser = parserA <|> parserB
      parser
        .lift(charTokenization("*a08*a07"))
        .assertMatch:
          case Some((_, 8)) =>

  test("optional parser succeeds on match"):
      val parser = intDigitParser.?
      parser
        .lift(charTokenization("1a"))
        .assertMatch:
          case Some((Token('a', _, _) #:: _, Some(1))) =>

  test("optional parser succeeds on lack of match"):
      val parser = intDigitParser.?
      parser
        .lift(charTokenization("a1"))
        .assertMatch:
          case Some((Token('a', _, _) #:: Token('1', _, _) #:: _, None)) =>

  test("optional parser can backtrack"):
      val parserA = just('*') +> letterParser +> digitParser +> letterParser
      val parserB = just('*') +> letterParser +> digitParser +> intDigitParser
      val parser = parserA.? <+> parserB
      parser
        .lift(charTokenization("*a08*a07"))
        .assertMatch:
          case Some((_, (None, 8))) =>
