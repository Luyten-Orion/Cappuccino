# Literal parsing
import std/[
  strformat,
  strutils
]

import ../lexer
import ./errors
import ./types

template ident(s: string): Identifier = types.Identifier(kind: AkIdentifier, name: s)

func expression*(p: Parser): Expression

func primary*(p: Parser): Expression =
  if p.peek.value == "true":
    p.advance()
    return Literal(kind: AkLiteral, litKind: Boolean, boolVal: true)

  elif p.peek.value == "false":
    p.advance()
    return Literal(kind: AkLiteral, litKind: Boolean, boolVal: false)

  elif p.peek.value == "nil":
    p.advance()
    return Literal(kind: AkLiteral, litKind: Nil)

  elif p.match({TokenType.String, TokenType.Char, Int, TokenType.Float}):
    if p.previous.typ == TokenType.String:
      return Literal(kind: AkLiteral, litKind: String, strVal: p.previous.value)

    elif p.previous.typ == TokenType.Char: # TODO: Implement character literal support
      return Literal(kind: AkLiteral, litKind: String, strVal: p.previous.value)

    elif p.previous.typ == Int:
      return Literal(kind: AkLiteral, litKind: Integer, intVal: parseInt(p.previous.value))

    elif p.previous.typ == TokenType.Float:
      return Literal(kind: AkLiteral, litKind: Float, floatVal: parseFloat(p.previous.value))

  elif p.match({OpenParen}):
    let e = p.expression()
    if not p.match({CloseParen}):
      raise newException(ZaphytParsingError, fmt"Expected ')' at line {p.previous.startLine} " &
        fmt"and column {p.previous.startColumn}!")
    return e


func unary*(p: Parser): Expression =
  if p.peek.value in ["not", "-"]:
    p.advance()

    let
      op = p.previous
      right = p.unary()

    return FunctionCall(kind: AkFunctionCall, function: op.value.ident, arguments: @[right])

  return p.primary()

func factor*(p: Parser): Expression =
  result = p.unary()

  while p.peek.value in ["*", "/", "%"]:
    p.advance()

    let
      op = p.previous
      right = p.unary()

    result = FunctionCall(kind: AkFunctionCall, function: op.value.ident, arguments: @[result, right])

func term*(p: Parser): Expression =
  result = p.factor()

  while p.peek.value in ["+", "-"]:
    p.advance()

    let
      op = p.previous
      right = p.factor()

    result = FunctionCall(kind: AkFunctionCall, function: op.value.ident, arguments: @[result, right])

func comparison*(p: Parser): Expression =
  result = p.term()

  while p.peek.value in ["<", ">", "<=", ">="]:
    p.advance()

    let
      op = p.previous
      right = p.term()

    result = FunctionCall(kind: AkFunctionCall, function: op.value.ident, arguments: @[result, right])

func equality*(p: Parser): Expression =
  result = p.comparison()

  while p.peek.value in ["==", "!="]:
    p.advance()

    let
      op = p.previous
      right = p.comparison()

    result = FunctionCall(kind: AkFunctionCall, function: op.value.ident, arguments: @[right])

func expression*(p: Parser): Expression = p.equality()

func parse*(p: Parser): Expression = p.expression()