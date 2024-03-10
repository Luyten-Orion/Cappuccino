import std/[
  strformat,
  strutils,
  streams,
  tables
]

type
  ZaphytLexingError* = object of CatchableError

  TokenType* = enum
    OpenParen, CloseParen, OpenBrace, CloseBrace, OpenBracket, CloseBracket, Identifier,
    Keyword, String, Char, Int, Float, Comma, Path, Colon, Semicolon, Indent, Dedent, EOF
    # Here we have a way to tell the difference between a keyword and identifer to make lexing
    # and parsing less hellish in theory

  Token* = object
    case typ*: TokenType
      of Path:
        subtokens*: seq[Token]
      of {Indent, Dedent, EOF}:
        discard
      else:
        value*: string
    startLine*, startColumn*: uint

  IndentationFormat = enum
    Unset = 0'u8, Space, Tab

  Lexer* = ref object
    stream: Stream
    line, column: uint
    indentFormat: IndentationFormat
    indentStack: seq[int]

const
  SimpleTokenMap = {
    ',': Comma,
    ':': Colon,
    ';': Semicolon,
    '(': OpenParen,
    ')': CloseParen,
    '[': OpenBracket,
    ']': CloseBracket,
    '{': OpenBrace,
    '}': CloseBrace
  }.toTable

  SpecialIdentifiers = ['+', '-', '*', '/', '&', '%', '^']

  Unidentifiers = ".,:+-*/&()[]{}\"'"

  Keywords = @[
    "func",
    "macro",
    "type",
    "object",
    "of",
    "ref",
    "static",
    "discard"
  ]

proc `$`*(token: Token, depth: int = 1): string =
  let indent = repeat("  ", depth)
  result &= &"Token(\n{indent}typ: {token.typ},\n"
  result &= &"{indent}startLine: {token.startLine},\n"
  result &= &"{indent}startColumn: {token.startColumn},\n"

  case token.typ
    of Path:
      result &= &"{indent}subtokens: @[\n"

      for subtoken in token.subtokens:
        result &= indent & "  " & `$`(subtoken, depth + 2)
        result.setLen(result.len - 2)
        result &= &"\n{indent}  ),\n"

      result.setLen(result.len - (indent.len + 6))
      result &= &"\n{indent}  )\n{indent}]"

    of {Indent, Dedent, EOF}:
      result.setLen(result.len - 2)

    else:
      result &= &"{indent}value: "
      result.addQuoted(token.value)

  result &= "\n)"

proc `$`*(tokens: seq[Token]): string =
  result = "@[\n"
  for token in tokens:
    result &= "  " & `$`(token, 2)
    result.setLen(result.len - 2)
    result &= "\n  ),\n"

  result.setLen(result.len - 2)

  result &= "\n]"

template atEnd(l: Lexer): bool = l.stream.atEnd()

template peek(l: Lexer): char = l.stream.peekChar()

proc next(l: Lexer) =
  ## Increments the column counter, and increments the line counter if
  ## a newline is encountered.
  if l.stream.readChar() == '\n':
    inc l.line
    l.column = 1
  else:
    inc l.column

proc newLexer*(stream: Stream): Lexer =
  ## Creates a new lexer from a stream.
  return Lexer(stream: stream, line: 1, column: 1, indentFormat: Unset)

proc lexStr(l: Lexer): Token =
  result = Token(typ: String, startLine: l.line, startColumn: l.column)
  var lexeme: string

  var
    cchar = l.peek()
    pchar: char

  l.next()

  while not l.atEnd:
    pchar = cchar
    cchar = l.peek()

    if cchar == '\n':
      raise newException(ZaphytLexingError,
        fmt"Unterminated string literal at line {l.line} and column {l.column}!")

    if (cchar == '"') and (pchar != '\\'):
      l.next()
      break

    result.value &= cchar

    l.next()

  result.value = lexeme

  if (cchar != '"') or (cchar == '"' and pchar == '\\'):
    raise newException(ZaphytLexingError,
      fmt"Unterminated string literal at line {l.line} and column {l.column}, reached EOF!")


proc lexNum(l: Lexer): Token =
  result.typ = Int

  result.value &= l.peek()
  l.next()

  result.startLine = l.line
  result.startColumn = l.column

  var dotCount: uint8 = 0

  while not l.atEnd:
    let c = l.peek()

    if (dotCount == 1) and (c == '.'):
      break

    elif (c == '.') and (l.peek().isDigit):
      result.typ = Float
      inc dotCount

    elif (c in Whitespace) or (not c.isDigit):
      break

    result.value &= c

    l.next()


proc lexBackticks(l: Lexer): Token =
  result = Token(typ: Identifier, startLine: l.line, startColumn: l.column)

  var
    cchar = l.peek()
    pchar: char

  l.next()

  while not l.atEnd:
    pchar = cchar
    cchar = l.peek()

    if cchar == '\n':
      raise newException(ZaphytLexingError,
        fmt"Unterminated backtick literal at line {l.line} and column {l.column}!")

    elif cchar != '`':
      result.value &= cchar

    elif (cchar == '`') and (pchar != '\\'):
      l.next()
      break

    l.next()

  if (cchar != '`') or (cchar == '`' and pchar == '\\'):
    raise newException(ZaphytLexingError,
      fmt"Unterminated backtick literal at line {l.line} and column {l.column}, reached EOF!")


proc lexIdent(l: Lexer): Token =
  var
    cchar = l.peek()
    lexeme: string
    tokenType = Identifier

  let
    startLine = l.line
    startColumn = l.column

  lexeme &= cchar

  l.next()

  while not l.atEnd:
    cchar = l.peek()

    if (cchar in Whitespace) or (cchar in Unidentifiers):
      if result.value in Keywords:
        tokenType = Keyword

      break

    lexeme &= cchar

    l.next()

  return if tokenType == Identifier:
    Token(typ: Identifier, startLine: startLine, startColumn: startColumn, value: lexeme)

  else:
    Token(typ: Keyword, startLine: startLine, startColumn: startColumn, value: lexeme)

proc lexUntil(l: Lexer, cond: (proc(l: Lexer): bool) = nil): seq[Token] # Forward decl

proc lexChar(l: Lexer): Token =
  var
    cchar = l.peek()
    pchar: char

  result = Token(typ: Char, startLine: l.line, startColumn: l.column)

  l.next()

  if l.atEnd:
    raise newException(ZaphytLexingError,
      fmt"Unterminated character literal at line {l.line} and column {l.column}!")

  else:
    pchar = cchar
    cchar = l.peek()

    if cchar == '\\':
      result.value &= cchar
      l.next()

      if l.atEnd:
        raise newException(ZaphytLexingError,
          fmt"Unterminated character literal at line {l.line} and column {l.column}!")

      else:
        pchar = cchar
        cchar = l.peek()

        if cchar == '\'' and pchar != '\\':
          raise newException(ZaphytLexingError,
            fmt"Unterminated character literal at line {l.line} and column {l.column}!")

        else:
          result.value.addQuoted(cchar)

          l.next()

    elif cchar == '\'':
      raise newException(ZaphytLexingError,
        fmt"Invalid character literal at line {l.line} and column {l.column}!")

    else:
      result.value &= cchar

      l.next()

  if l.atEnd:
    raise newException(ZaphytLexingError,
      fmt"Unterminated character literal at line {l.line} and column {l.column}!")

  else:
    pchar = cchar
    cchar = l.peek()

    if cchar != '\'':
      raise newException(ZaphytLexingError,
        fmt"Unterminated character literal at line {l.line} and column {l.column}!")

  l.next()

proc lexPath(l: Lexer, prevToken: Token): Token =
  var
    cchar = l.peek()
    pchar: char

  result = Token(typ: Path, startLine: l.line, startColumn: l.column, subtokens: @[prevToken])

  while not l.atEnd:
    pchar = cchar
    cchar = l.peek()

    if cchar == '.':
      l.next()
      result.subtokens.add l.lexUntil(proc(l: Lexer): bool = l.peek() notin Unidentifiers)

      var ccchar = l.peek()
      if (ccchar in {'(', ')', '[', ']', '{', '}'}) or (ccchar in SpecialIdentifiers):
        break

    else:
      break

proc lexIndent(l: Lexer): seq[Token] =
  let
    startLine = l.line
    startColumn = l.column

  var
    cchar: char
    indentDepth = 0

  while not l.atEnd:
    cchar = l.peek()

    if cchar in {'\t', ' '}:
      discard

    elif cchar in Whitespace:
      return

    else:
      break

    case l.indentFormat
      of Unset:
        if cchar == '\t':
          l.indentFormat = Tab
        elif cchar == ' ':
          l.indentFormat = Space
        else:
          raise newException(ZaphytLexingError,
            fmt"Invalid indentation character at line {l.line} and column {l.column}, got `{cchar}`!")

        indentDepth += 1

      of Tab:
        if cchar == '\t':
          indentDepth += 1
        else:
          raise newException(ZaphytLexingError,
            fmt"Invalid indentation character at line {l.line} and column {l.column}, expected a tab but " &
              fmt"got '{cchar}'!")

      of Space:
        if cchar == ' ':
          indentDepth += 1
        else:
          raise newException(ZaphytLexingError,
            fmt"Invalid indentation character at line {l.line} and column {l.column}, expected a space but " &
              fmt"got '{cchar}'!")

    l.next()

  if l.indentStack.len == 0:
    l.indentStack.add indentDepth
    return @[Token(typ: Indent, startLine: startLine, startColumn: startColumn)]

  if indentDepth > l.indentStack[^1]:
    l.indentStack.add indentDepth
    return @[Token(typ: Indent, startLine: startLine, startColumn: startColumn)]

  else:
    if indentDepth != 0:
      let indexOfMatchingIndent = l.indentStack.find(indentDepth)

      if indexOfMatchingIndent == -1:
        raise newException(ZaphytLexingError,
          fmt"Invalid dedentation at line {l.line} and column {l.column}!")

      let toPop = l.indentStack[indexOfMatchingIndent..<l.indentStack.len].len

      for i in 0..<toPop:
        discard l.indentStack.pop()
        result.add Token(typ: Dedent, startLine: startLine, startColumn: startColumn)

    else:
      for i in 0..l.indentStack.len:
        result.add Token(typ: Dedent, startLine: startLine, startColumn: startColumn)

      l.indentStack.setLen(0)


proc lexUntil(l: Lexer, cond: (proc(l: Lexer): bool) = nil): seq[Token] =
  template condition(): bool =
    if cond == nil:
      not l.atEnd
    else:
      cond(l) and not l.atEnd

  while condition():
    let cchar = l.peek()

    if cchar == '\n':
      l.next()
      result.add l.lexIndent()

    elif cchar in Whitespace:
      l.next()

    elif cchar == '"':
      result.add l.lexStr()

    elif cchar == '`':
      result.add l.lexBackticks()

    elif cchar == '\'':
      result.add l.lexChar()

    elif cchar.isDigit():
      result.add l.lexNum()

    elif cchar == '.':
      if result.len == 0:
        raise newException(ZaphytLexingError,
          fmt"Path token found at line {l.line}, column {l.column}, without a token defined before it!")

      result.add l.lexPath(result.pop())

    elif cchar in SimpleTokenMap:
      result.add Token(startLine: l.line, startColumn: l.column, typ: SimpleTokenMap[cchar], value: $cchar)
      l.next()

    elif cchar in SpecialIdentifiers:
      var
        token = Token(startLine: l.line, startColumn: l.column, typ: Identifier)
        c: char
      token.value &= cchar

      l.next()

      while not l.atEnd:
        c = l.peek()
        l.next()
        if c in SpecialIdentifiers:
          token.value &= c

        else:
          break

      result.add token

    else:
      result.add l.lexIdent()

proc lex*(l: Lexer): seq[Token] =
  result = l.lexUntil()

  for i in 0..<l.indentStack.len:
    result.add Token(startLine: l.line, startColumn: l.column, typ: Dedent)

  result.add Token(startLine: l.line, startColumn: l.column, typ: EOF)