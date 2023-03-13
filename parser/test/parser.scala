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

  test("repeating parser can accept multiple elements"):
      val parser = intDigitParser.*
      parser
        .lift(charTokenization("12345a"))
        .assertMatch:
          case Some((Token('a', _, _) #:: _, List(1, 2, 3, 4, 5))) =>

  test("repeating parser can accept single element"):
      val parser = intDigitParser.*
      parser
        .lift(charTokenization("1a"))
        .assertMatch:
          case Some((Token('a', _, _) #:: _, List(1))) =>

  test("repeating parser can accept zero elements"):
      val parser = intDigitParser.*
      parser
        .lift(charTokenization("a1"))
        .assertMatch:
          case Some((Token('a', _, _) #:: _, Nil)) =>

  test("repeating parser can backtrack"):
      val prefix = (just('$') +> intDigitParser).*
      val parser = prefix <+> (just('$') +> letterParser)
      parser
        .lift(charTokenization("$1$2$3$a"))
        .assertMatch:
          case Some(t, (List(1, 2, 3), 'a')) if t.isEmpty =>

  test("repeating parser can backtrack on zero matches"):
      val prefix = (just('$') +> intDigitParser).*
      val parser = prefix <+> (just('$') +> letterParser)
      parser
        .lift(charTokenization("$a"))
        .assertMatch:
          case Some(t, (Nil, 'a')) if t.isEmpty =>

  test("non-empty repeating parser can accept multiple elements"):
      val parser = intDigitParser.+
      parser
        .lift(charTokenization("12345a"))
        .assertMatch:
          case Some((Token('a', _, _) #:: _, List(1, 2, 3, 4, 5))) =>

  test("non-empty repeating parser can accept single element"):
      val parser = intDigitParser.+
      parser
        .lift(charTokenization("1a"))
        .assertMatch:
          case Some((Token('a', _, _) #:: _, List(1))) =>

  test("non-empty repeating parser cannot accept zero elements"):
      val parser = intDigitParser.+
      parser
        .lift(charTokenization("a1"))
        .assertMatch:
          case None =>

  test("non-empty repeating parser can backtrack"):
      val prefix = (just('$') +> intDigitParser).+
      val parser = prefix <+> (just('$') +> letterParser)
      parser
        .lift(charTokenization("$1$2$3$a"))
        .assertMatch:
          case Some(t, (List(1, 2, 3), 'a')) if t.isEmpty =>

  test("non-empty repeating parser cannot backtrack on zero matches"):
      val prefix = (just('$') +> intDigitParser).+
      val parser = prefix <+> (just('$') +> letterParser)
      parser
        .lift(charTokenization("$a"))
        .assertMatch:
          case None =>

  test("separated parser can parse list"):
      val parser = intDigitParser.separatedBy(just(','))
      parser
        .lift(charTokenization("1,2,3,4"))
        .assertMatch:
          case Some(_, List(1, 2, 3, 4)) =>

  test("separated parser can parse single element list"):
      val parser = intDigitParser.separatedBy(just(','))
      parser
        .lift(charTokenization("1"))
        .assertMatch:
          case Some(_, List(1)) =>

  test("separated parser can ignore trailing separator"):
      val parser = intDigitParser.separatedBy(just(','))
      parser
        .lift(charTokenization("1,2,3,4,"))
        .assertMatch:
          case Some(Token(',', _, _) #:: _, List(1, 2, 3, 4)) =>

  test("separated parser can consume trailing separator"):
      val parser = intDigitParser.separatedBy(just(','), trailingOpt = true)
      parser
        .lift(charTokenization("1,2,3,4,"))
        .assertMatch:
          case Some(rest, List(1, 2, 3, 4)) if rest.isEmpty =>

  test("separated parser passes without trialing separator even when enabled"):
      val parser = intDigitParser.separatedBy(just(','), trailingOpt = true)
      parser
        .lift(charTokenization("1,2,3,4"))
        .assertMatch:
          case Some(rest, List(1, 2, 3, 4)) if rest.isEmpty =>

  test("separated parser passes on empty list"):
      val parser = intDigitParser.separatedBy(just(','))
      parser
        .lift(charTokenization("a"))
        .assertMatch:
          case Some(_, List()) =>

  test(
    "separated parser pases on empty list when trailing separator is enabled"
  ):
      val parser = intDigitParser.separatedBy(just(','), trailingOpt = true)
      parser
        .lift(charTokenization("a"))
        .assertMatch:
          case Some(_, List()) =>

  test("separated parser do not comsumes trialing separator only"):
      val parser = intDigitParser.separatedBy(just(','), trailingOpt = true)
      parser
        .lift(charTokenization(",a"))
        .assertMatch:
          case Some(Token(',', _, _) #:: _, List()) =>

  test("separated parser do not consume list starting with separator"):
      val parser = intDigitParser.separatedBy(just(','))
      parser
        .lift(charTokenization(",1,2,3"))
        .assertMatch:
          case Some(Token(',', _, _) #:: _, List()) =>

  test(
    "separated parser do not consume list starting with separator even wit trailingOpt"
  ):
      val parser = intDigitParser.separatedBy(just(','), trailingOpt = true)
      parser
        .lift(charTokenization(",1,2,3"))
        .assertMatch:
          case Some(Token(',', _, _) #:: _, List()) =>
