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
  let val = p.peek()

  case val.typ
    of Int:
      result = Literal(kind: AkLiteral, litKind: Integer, intVal: parseInt(val.value))

    of String:
      result = Literal(kind: AkLiteral, litKind: String, strVal: val.value)

    of Float:
      result = Literal(kind: AkLiteral, litKind: Float, floatVal: parseFloat(val.value))

    of Identifier:
      if val.value == "true":
        result = Literal(kind: AkLiteral, litKind: Boolean, boolVal: true)

      elif val.value == "false":
        result = Literal(kind: AkLiteral, litKind: Boolean, boolVal: false)

      else:
        result = Literal(kind: AkLiteral, litKind: types.Identifier, strVal: val.value)


func parse*(p: Parser): Grouping =
  result = Grouping(kind: AkGrouping)

  let exr = p.peek()

  if exr.typ == Int:
    result.child = Literal(kind: AkLiteral, litKind: Integer, intVal: parseInt(exr.value))
    p.advance()
  
  elif exr.typ == String:
    result.child = Literal(kind: AkLiteral, litKind: String, strVal: exr.value)
    p.advance()