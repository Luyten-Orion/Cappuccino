import std/[unittest, streams]

import cappuchino/lexer

test "Number lexing":
  echo currentSourcePath()
  let lex = newLexer(newFileStream("tests/res/num.chno"))
  let tokens = lex.lex()

  check tokens == @[
    Token(
      typ: Int,
      startLine: 1,
      startColumn: 2,
      value: "427321"
    ),
    Token(
      typ: EOF,
      startLine: 1,
      startColumn: 7
    )
  ]

test "String lexing":
  let lex = newLexer(newFileStream("tests/res/string.chno"))
  let tokens = lex.lex()

  check tokens == @[
    Token(
      typ: String,
      startLine: 1,
      startColumn: 1,
      value: "Hello, World!"
    ),
    Token(
      typ: EOF,
      startLine: 1,
      startColumn: 16
    )
  ]

test "Math lexing":
  let lex = newLexer(newFileStream("tests/res/math.chno"))
  let tokens = lex.lex()

  check tokens == @[
    Token(
      typ: Int,
      startLine: 1,
      startColumn: 2,
      value: "1"
    ),
    Token(
      typ: Identifier,
      startLine: 1,
      startColumn: 3,
      value: "+"
    ),
    Token(
      typ: Int,
      startLine: 1,
      startColumn: 6,
      value: "2"
    ),
    Token(
      typ: Identifier,
      startLine: 1,
      startColumn: 7,
      value: "*"
    ),
    Token(
      typ: Int,
      startLine: 1,
      startColumn: 10,
      value: "3"
    ),
    Token(
      typ: Identifier,
      startLine: 1,
      startColumn: 11,
      value: "/"
    ),
    Token(
      typ: Int,
      startLine: 1,
      startColumn: 14,
      value: "6"
    ),
    Token(
      typ: EOF,
      startLine: 1,
      startColumn: 14
    )
  ]

test "Method call lexing":
  let lex = newLexer(newFileStream("tests/res/methodCall.chno"))
  let tokens = lex.lex()

  check tokens == @[
    Token(
      typ: Identifier,
      startLine: 1,
      startColumn: 1,
      value: "writeln"
    ),
    Token(
      typ: OpenParen,
      startLine: 1,
      startColumn: 8,
      value: "("
    ),
    Token(
      typ: Identifier,
      startLine: 1,
      startColumn: 9,
      value: "stdout"
    ),
    Token(
      typ: Comma,
      startLine: 1,
      startColumn: 15,
      value: ","
    ),
    Token(
      typ: String,
      startLine: 1,
      startColumn: 17,
      value: "Hello, World!"
    ),
    Token(
      typ: CloseParen,
      startLine: 1,
      startColumn: 32,
      value: ")"
    ),
    Token(
      typ: EOF,
      startLine: 1,
      startColumn: 33
    )
  ]

test "Method call path lexing":
  let lex = newLexer(newFileStream("tests/res/methodCallPathAccess.chno"))
  let tokens = lex.lex()

  check tokens == @[
    Token(
      typ: Path,
      startLine: 1,
      startColumn: 7,
      subtokens: @[
        Token(
          typ: Identifier,
          startLine: 1,
          startColumn: 1,
          value: "stdout"
        ),
        Token(
          typ: Identifier,
          startLine: 1,
          startColumn: 8,
          value: "writeln"
        )
      ]
    ),
    Token(
      typ: OpenParen,
      startLine: 1,
      startColumn: 17,
      value: "("
    ),
    Token(
      typ: String,
      startLine: 1,
      startColumn: 18,
      value: "Hello, World!"
    ),
    Token(
      typ: CloseParen,
      startLine: 1,
      startColumn: 33,
      value: ")"
    ),
    Token(
      typ: EOF,
      startLine: 1,
      startColumn: 34
    )
  ]

test "Method call path access multiline lexing":
  let lex = newLexer(newFileStream("tests/res/methodCallPathAccessMultiline.chno"))
  let tokens = lex.lex()

  check tokens == @[
    Token(
      typ: Path,
      startLine: 1,
      startColumn: 7,
      subtokens: @[
        Token(
          typ: Identifier,
          startLine: 1,
          startColumn: 1,
          value: "stdout"
        ),
        Token(
          typ: Indent,
          startLine: 2,
          startColumn: 1
        ),
        Token(
          typ: Identifier,
          startLine: 2,
          startColumn: 3,
          value: "writeln"
        )
      ]
    ),
    Token(
      typ: OpenParen,
      startLine: 2,
      startColumn: 12,
      value: "("
    ),
    Token(
      typ: String,
      startLine: 2,
      startColumn: 13,
      value: "Hello, World!"
    ),
    Token(
      typ: CloseParen,
      startLine: 2,
      startColumn: 28,
      value: ")"
    ),
    Token(
      typ: Dedent,
      startLine: 2,
      startColumn: 29
    ),
    Token(
      typ: EOF,
      startLine: 2,
      startColumn: 29
    )
  ]