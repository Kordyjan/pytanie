package pytanie.parser

def graphqlTokenizer: String => Tokens[String] =
  def process(s: String, i: Int): Option[Token[String]] =
    var start = i
    while start < s.length && s(start).isWhitespace do start += 1
    if start == s.length then None
    else
      s(start).match
        case '#' =>
          var end = start
          while end < s.length && s(end) != '\n' do end += 1
          process(s, end)
        case '"' =>
          var end = start + 1
          while end < s.length && s(end) != '"' do end += 1
          Some(Token(s.substring(start, end + 1), start, end + 1))
        case c if c.isLetterOrDigit =>
          var end = start
          while end < s.length && s(end).isLetterOrDigit do end += 1
          Some(Token(s.substring(start, end), start, end))
        case '.' if s.substring(start, start + 3) == "..." =>
          Some(Token("...", start, start + 3))
        case c =>
          Some(Token(c.toString, start, start + 1))

  tokenize(process)
