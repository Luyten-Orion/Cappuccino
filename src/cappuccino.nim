import std/tables

import ./cappuccino/[combinator, results, nodes]

type
  PrecedencePower* = distinct int

template `@`*(p: int): PrecedencePower = PrecedencePower(p)

const
  NoPrecedence = @ -1

  PrecedenceTable: Table[string, tuple[unary: PrecedencePower, infix: PrecedencePower]] = {
    # TODO: Shift these down by 4 points?
    "@": (@12, NoPrecedence),
    ".": (NoPrecedence, @11),
    "$": (@10, NoPrecedence),
    "^": (@10, @4), # Exponentiation and bitop
    "&": (@10, @10),
    "/": (NoPrecedence, @9),
    "*": (NoPrecedence, @8),
    ">>": (NoPrecedence, @8),
    "<<": (NoPrecedence, @8),
    "+": (@0, @8),
    "-": (@0, @8),
    "..": (NoPrecedence, @7),
    "==": (NoPrecedence, @6),
    "!=": (NoPrecedence, @6),
    ">": (NoPrecedence, @6),
    ">=": (NoPrecedence, @6),
    "<": (NoPrecedence, @6),
    "<=": (NoPrecedence, @6),
    "in": (NoPrecedence, @6),
    "notin": (NoPrecedence, @6),
    "is": (NoPrecedence, @6),
    "isnot": (NoPrecedence, @6),
    "not": (NoPrecedence, @6),
    "of": (NoPrecedence, @6),
    "as": (NoPrecedence, @6),
    "and": (NoPrecedence, @5), # Boolean algebra
    "&": (NoPrecedence, @5), # Bitops
    "or": (NoPrecedence, @4), # Boolean algebra
    "|": (NoPrecedence, @4), # Bitops
    "xor": (NoPrecedence, @4),
    "=": (NoPrecedence, @4),
  }.toTable

const parser = newlineLocator and parseNumber and skipWhitespace and parseAccQuote
let res = parser(ParserState.init(), "0b110 `a_cd`", 0)

echo res