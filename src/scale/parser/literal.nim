# Literal parsing
import std/[
  strformat,
  strutils
]

import ../lexer
import ./errors
import ./types

const Literals = {Int, String, Float}

template ident(s: string): Identifier = types.Identifier(kind: AkIdentifier, name: s)

func parseLiteral*(p: Parser): Literal =

  result = Literal(kind: AkLiteral, litKind: String, strVal: p.peek().value)
  p.advance()

func parse*(p: Parser): Grouping =
  result = Grouping(kind: AkGrouping)

  let exr = p.peek()

  if exr.typ == Int:
    result.child = Literal(kind: AkLiteral, litKind: Integer, intVal: parseInt(exr.value))
    assert result.child.Literal.intVal == 427321
    p.advance()
  
  elif exr.typ == String:
    result.child = Literal(kind: AkLiteral, litKind: String, strVal: exr.value)
    assert result.child.Literal.strVal == "Hello, World!"
    p.advance()