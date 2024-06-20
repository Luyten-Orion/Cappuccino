import std/[typetraits, parseutils, genasts, macros]

import pkg/[results]

import ./nodes


const
  ## The set of characters that can be used as an identifier's starting character
  IdentifierInitialChars* = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz"
  IdentifierChars* = IdentifierInitialChars & "0123456789_"
  SpecialIdentifierChars* = "<>,:;!?@$%^&/*+-=~|"
  #UnidentifierChars* = "()[]{}\"'\\`"


type
  IntFormat = enum
    Dec, Hex, Bin

  ParserCombinationDefect* = object of Defect

  ParseFailureKind* = enum
    Unknown, EndOfInput, ExpectedWhitespace, CannotDetectIndentationType, InconsistentIndentationSpacing,
    NotAnIdentifier, ExpectedChar

  ParseFailure* = object
    position*: Natural

    case kind*: ParseFailureKind
    of {Unknown, EndOfInput, ExpectedWhitespace, CannotDetectIndentationType, InconsistentIndentationSpacing}:
      discard
    of NotAnIdentifier:
      naiChar*: char
    of ExpectedChar:
      ecFoundChar*, ecExpectedChar*: char

  IndentType* = enum
    itUnset, itSpaces, itTabs

  ParserState* = object
    astIdx: AstNodeIndex
    indentStack: seq[int] = @[0]
    indentType: IndentType = itUnset

  ParseSuccess* = object
    state: ParserState
    position*: Natural
    nodes*: seq[AstNode]

  ParseResult* = Result[ParseSuccess, ParseFailure]

  Parser* = proc(state: ParserState, input: openArray[char], position: Natural = 0): ParseResult {.noSideEffect, gcsafe.}

# Flatten proc
proc flatten(a, b: ParseSuccess): ParseSuccess =
  ## Flattens two `ParseSuccess` objects into one, requires copying, is there a better way without using a state?
  result = ParseSuccess(
    state: b.state,
    position: b.position,
    nodes: newSeqOfCap[AstNode](a.nodes.len + a.nodes.len)
  )

  result.nodes = a.nodes & b.nodes

# 'Call' just calls `p` and returns the result, flattening as needed
template call(result: ParseResult, p: Parser, input: openArray[char], errBody: typed, errBodyOnFail: bool = true): ParseResult =
  ## Calls `p` on `input` and returns the result, using `res` for the offset
  if result.isOk:
    let res {.inject.} = p(result.unsafeGet().state, input, result.unsafeGet().position)
    if res.isOk:
      ok(flatten(result.get(), res.get()))
    else:
      if errBodyOnFail:
        errBody
      else:
        res
  else:
    errBody

template call(result: ParseResult, p: Parser, input: openArray[char], errBodyOnFail: bool = true): ParseResult =
  ## Calls `p` on `input` and returns the result, using `res` for the offset
  call(result, p, input, result, errBodyOnFail)


proc stringify(input: openArray[char]): string =
  ## Helper function that converts `openArray[char]` to a `string`
  result = newStringOfCap(input.len)
  for i in input: result.add i


proc parseIndent*(state: ParserState, input: openArray[char], position: Natural = 0): ParseResult =
  ## Parses an indent, input at offset must start with a newline.
  ## It'll only parse spaces or tabs, once it detects one or the other.
  var
    offset = position
    counter = 0
    currentState = state

  while input.len > offset:
    if input[offset] != '\n': return err(ParseFailure(kind: ExpectedChar, ecFoundChar: input[offset], ecExpectedChar: '\n'))
    while input.len > offset and input[offset] == '\n':
      inc offset

    if offset >= input.len:
      if currentState.indentStack.len != 1:
        currentState.indentStack.setLen(1)
        currentState.astIdx += 1
        return ok(ParseSuccess(state: currentState, position: offset, nodes: @[Dedent.init(currentState.indentStack[0])]))
      return ok(ParseSuccess(state: currentState, position: offset, nodes: @[]))

    if currentState.indentType == itUnset:
      case input[offset]
      of ' ':
        currentState.indentType = itSpaces
      of '\t':
        currentState.indentType = itTabs
      else:
        if currentState.indentStack.len > 1:
          currentState.indentStack.setLen 1

    let ichar = case currentState.indentType
    of itSpaces:
      ' '
    of itTabs:
      '\t'
    else:
      return err(ParseFailure(kind: CannotDetectIndentationType))

    while input.len >= offset and input[offset] in {' ', '\t'}:
      if input[offset] == ichar:
        inc counter
        inc offset
      else:
        return err(ParseFailure(kind: ExpectedChar, ecFoundChar: input[offset], ecExpectedChar: ichar))

    if input[offset] == '\n':
      counter = 0
      continue

    let lastISLen = currentState.indentStack.len

    if counter > currentState.indentStack[^1]:
      currentState.indentStack.add counter
    elif counter < currentState.indentStack[^1]:
      let i = currentState.indentStack.find(counter)
      if i == -1:
        return err(ParseFailure(kind: InconsistentIndentationSpacing))
      while counter < currentState.indentStack[^1]:
        if currentState.indentStack.len == 1:
          break
        currentState.indentStack.delete currentState.indentStack.len - 1
    else:
      return err(ParseFailure(kind: InconsistentIndentationSpacing))


    var
      idx = currentState.astIdx
      node: AstNode = if currentState.indentStack.len > lastISLen:
        Indent.init(currentState.indentStack.len).AstNode
      elif currentState.indentStack.len < lastISLen:
        Dedent.init(currentState.indentStack.len).AstNode
      else:
        return ok(ParseSuccess(state: currentState, position: offset, nodes: @[]))

    currentState.astIdx += 1
    result = ok(ParseSuccess(state: currentState, position: offset, nodes: @[node]))
    break


proc parseIdentifier*(state: ParserState, input: openArray[char], position: Natural = 0): ParseResult =
  ## Parses an identifier from `input` starting at `position`
  var offset = position

  if offset >= input.len: return err(ParseFailure(kind: EndOfInput, position: offset))
  elif input[offset] notin IdentifierInitialChars:
    return err(ParseFailure(kind: NotAnIdentifier, naiChar: input[offset], position: offset))

  while offset < input.len and input[offset] in IdentifierChars: inc offset

  ok(ParseSuccess(state: state, position: offset, nodes: @[Identifier.init(input[position..<offset].stringify)]))


proc parseAccQuote*(state: ParserState, input: openArray[char], position: Natural = 0): ParseResult =
  ## Parses an accQuote from `input` starting at `position`
  var offset = position

  if offset >= input.len: return err(ParseFailure(kind: EndOfInput, position: offset))

  if input[offset] != '`':
    return err(ParseFailure(kind: ExpectedChar, ecFoundChar: input[offset], ecExpectedChar: '`', position: offset))
  inc offset

  while offset < input.len and input[offset] != '`':
    if input[offset] == '\n':
      return err(ParseFailure(kind: ExpectedChar, ecFoundChar: input[offset], ecExpectedChar: '`', position: offset))
    inc offset

  if offset >= input.len: return err(ParseFailure(kind: EndOfInput, position: offset))

  if input[offset] != '`':
    return err(ParseFailure(kind: ExpectedChar, ecFoundChar: input[offset], ecExpectedChar: '`', position: offset))
  inc offset

  ok(ParseSuccess(state: state, position: offset, nodes: @[AccQuote.init(input[(position + 1)..<(offset - 1)].stringify)]))


proc parseString*(state: ParserState, input: openArray[char], position: Natural = 0): ParseResult =
  ## Parses a string from `input` starting at `position`
  var offset = position

  if offset >= input.len: return err(ParseFailure(kind: EndOfInput, position: offset))

  if input[offset] != '"':
    return err(ParseFailure(kind: ExpectedChar, ecFoundChar: input[offset], ecExpectedChar: '"', position: offset))
  inc offset

  while offset < input.len and input[offset] != '"':
    if input[offset] == '\n':
      return err(ParseFailure(kind: ExpectedChar, ecFoundChar: input[offset], ecExpectedChar: '`', position: offset))
    inc offset

  if offset >= input.len: return err(ParseFailure(kind: EndOfInput, position: offset))

  if input[offset] != '"':
    return err(ParseFailure(kind: ExpectedChar, ecFoundChar: input[offset], ecExpectedChar: '"', position: offset))
  inc offset

  ok(ParseSuccess(state: state, position: offset, nodes: @[String.init(input[(position + 1)..<(offset - 1)].stringify)]))


proc parseNumber*(state: ParserState, input: openArray[char], position: Natural = 0): ParseResult =
  ## Parses a number from `input` starting at `position`
  ##
  ## TODO: Handle overflow
  var
    offset = position
    numFormat = Dec
    isFloat = false

  if offset > input.len: return err(ParseFailure(kind: EndOfInput, position: offset))

  if input[offset] == '0' and (offset + 1) < input.len:
    if input[offset + 1] in {'x', 'X'}:
      numFormat = Hex
      inc offset
      inc offset

    elif input[offset + 1] in {'b', 'B'}:
      numFormat = Bin
      inc offset
      inc offset

  var
    intVal: int # TODO: int128
    floatVal: float64

  discard case numFormat
  of Dec:
    while offset < input.len and input[offset] in {'0'..'9', '_'}: inc offset
    if offset >= input.len or input[offset] != '.':
      input[position..<offset].parseInt(intVal)
    else:
      isFloat = true
      while offset < input.len and input[offset] in {'0'..'9', '_'}: inc offset
      input[position..<offset].parseFloat(floatVal)
  of Hex:
    while offset < input.len and input[offset] in {'0'..'9', 'a'..'f', 'A'..'F', '_'}: inc offset
    input[position..<offset].parseHex(intVal)
  of Bin:
    while offset < input.len and input[offset] in {'0'..'1', '_'}: inc offset
    input[position..<offset].parseBin(intVal)

  if offset < input.len and input[offset] != ' ':
    return err(ParseFailure(kind: ExpectedWhitespace, position: offset))

  if isFloat:
    ok(ParseSuccess(state: state, position: offset, nodes: @[Float.init(floatVal)]))
  else:
    ok(ParseSuccess(state: state, position: offset, nodes: @[Int.init(intVal)]))


proc charParser*(c: char): Parser =
  ## Creates a parser that parses a single character `c`
  proc parseCharacter(state: ParserState, input: openArray[char], position: Natural = 0): ParseResult =
    ## Creates a parser that parses a single character `c`
    if position >= input.len: return err(ParseFailure(kind: EndOfInput, position: position))
    elif input[position] == c: ok(ParseSuccess(state: state, position: position + 1, nodes: @[]))
    else: err(ParseFailure(kind: ExpectedChar, ecExpectedChar: c, ecFoundChar: input[position], position: position))

  parseCharacter


macro `and`*(a, b: Parser): Parser =
  ## Combines parsers `a` and `b`, running `b` only if `a` succeeds.
  genAst(a, b, andCombinator=genSym(nskProc, "andCombinator"), result=ident("result")) do:
    proc andCombinator(state: ParserState, input: openArray[char], position: Natural = 0): ParseResult =
      ## Runs parsers `a` and `b`, running `b` only if `a` succeeds.
      result = a(state, input, position)
      result = result.call(b, input, errBodyOnFail=false)

    andCombinator


macro `or`*(a, b: Parser): Parser =
  ## Combines parsers `a` and `b`, running `b` after `a` only if `a` fails.
  genAst(a, b, orCombinator=genSym(nskProc, "orCombinator"), result=ident("result")) do:
    proc orCombinator(state: ParserState, input: openArray[char], position: Natural = 0): ParseResult =
      ## Runs parsers `a` or `b`, running `b` only if `a` fails.
      result = a(state, input, position)
      if result.isErr:
        result = b(state, input, position)

    orCombinator


proc link*(ps: varargs[Parser]): Parser =
  ## Combines all parsers provided into one using the same logic as `and`.
  if ps.len < 2:
    raise newException(ParserCombinationDefect, "`link` requires at least two parsers to join together.")

  let parsers = @ps

  proc linkCombinator(state: ParserState, input: openArray[char], position: Natural = 0): ParseResult =
    ## Combines parsers `a` and `b`, running `b` only if `a` fails.
    result = parsers[0](state, input, position)

    for p in parsers[1..^1]:
      result = result.call(p, input)

  linkCombinator


macro then*(p: Parser, input: untyped{nkIdent}, body): Parser =
  ## Creates a parser that runs `p` and then any code passed to the `body`
  let thenBlock = genSym(nskProc, "thenBlock")

  genAst(thenBlock, result=ident("result")) do:
    proc thenBlock(state: ParserState, input: openArray[char], position: Natural = 0): ParseResult =
      ## Runs the given parser, and on success then runs `body`.
      result = p(state, input, position)

      if result.isOk:
        body

    thenBlock


macro label*(p: Parser, name: static string): Parser =
  ## Creates a parser that creates a function with the specified `name` and then executes `p`
  let functionName = genSym(nskProc, name)

  genAst(p, functionName, result=ident("result")) do:
    proc functionName(state: ParserState, input: openArray[char], position: Natural = 0): ParseResult =
      ## A labelled parser that runs the given parser.
      result = p(state, input, position)

    functionName


proc lift*(p: ParseResult): Parser =
  ## Generates a parser that returns the given `ParseResult`, lifting it.
  proc liftCombinator(state: ParserState, input: openArray[char], position: Natural = 0): ParseResult =
    ## Returns the given `ParseResult`, lifting it.
    result = p

  liftCombinator


proc optional*(p: Parser): Parser =
  ## Creates a parser that runs `p` exactly once, but still returns okay if the parsing fails
  proc optionalCombinator(state: ParserState, input: openArray[char], position: Natural = 0): ParseResult =
    ## Runs `p` exactly once, but still returns okay if the parsing fails
    result = p(state, input, position)
    if result.isErr:
      result = ok(ParseSuccess(state: state, position: position, nodes: @[]))

  optionalCombinator

proc many0*(p: Parser): Parser =
  ## Creates a parser that runs `p` 0 or more times
  proc many0Combinator(state: ParserState, input: openArray[char], position: Natural = 0): ParseResult =
    ## Runs `p` 0 or more times
    result = ok(ParseSuccess(state: state, position: position, nodes: @[]))

    while result.isOk:
      let res = result.call(p, input): break
      result = res

  many0Combinator


proc many1*(p: Parser): Parser =
  ## Creates a parser that runs `p` 1 or more times
  proc many1Combinator(state: ParserState, input: openArray[char], position: Natural = 0): ParseResult =
    ## Runs `p` 1 or more times
    result = p(state, input, position)

    while result.isOk:
      let res = result.call(p, input) do: break
      result = res

  many1Combinator

const skipWhitespace* = static(many0(charParser(' ')).label("skipWhitespace"))