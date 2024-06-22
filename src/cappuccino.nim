import std/tables

import pkg/results

import ./cappuccino/[combinator, nodes]

func parseExpression*(state: sink ParserState, input: openArray[char], position: Natural): ParseResult =
  ## Parses an expression from `input` starting at `position`.
  var
    offset = position
    currentState = state

  const parseLiteral = label(parseNumber, "parseLiteral", "Couldn't parse a literal at {lineInfo}!")

const parser = newlineLocator and parseNumber and skipWhitespace and many1(parseIdentifier)
let res = parser(ParserState.init(), "0b110 a_cd-ab", 0)

echo res