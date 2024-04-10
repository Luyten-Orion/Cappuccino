# Literal parsing
import std/[
  strformat,
  strutils
]

import ../lexer
import ./errors
import ./types

const
  Literals = {Int, String, Float, Char}
  SpecialIdentifiers = ['+', '-', '*', '/', '&', '%', '^']

template ident(s: string): Identifier = types.Identifier(kind: AkIdentifier, name: s)

proc isOperator(s: string): bool =
  result = true

  for c in s:
    if c notin SpecialIdentifiers:
      return false

func parseLiteral*(p: Parser): Literal =
  let val = p.peek()

  case val.typ
    of Char:
      result = Literal(kind: AkLiteral, litKind: Character, chrVal: val.value)

    of String:
      result = Literal(kind: AkLiteral, litKind: String, strVal: val.value)

    of Int:
      result = Literal(kind: AkLiteral, litKind: Integer, intVal: parseInt(val.value))

    of Float:
      result = Literal(kind: AkLiteral, litKind: Float, floatVal: parseFloat(val.value))

    else:
      raise newException(CappuccinoParsingError, &"Unimplemented literal for type: {val.typ}")

  result.lineInfo = CnLineInfo(line: val.startLine, column: val.startColumn)

  p.advance()

func parseIdentifier*(p: Parser): Identifier =
  let token = p.peek()

  if token.typ != TokenType.Identifier:
    raise newException(CappuccinoParsingError, &"Expected identifier, got {token.typ}!")

  result = ident(token.value)
  result.lineInfo = CnLineInfo(line: token.startLine, column: token.startColumn)

  p.advance()

func parse*(p: Parser): Grouping =
  result = Grouping(kind: AkGrouping)

  let exr = p.peek()

  if exr.typ in Literals or exr.typ == Identifier:
    if exr.typ != Identifier:
      result.child = parseLiteral(p)
    else:
      result.child = parseIdentifier(p)

    if exr.typ == Identifier:
      if not exr.value.isOperator:
        raise newException(CappuccinoParsingError, &"Expected operator, got {exr.value}.")

      let ident = parseIdentifier(p)

      result.child = 