import std/[parseutils, strutils, genasts, tables, macros]

import pkg/results

import ./nodes


const
  ## The set of characters that can be used as an identifier's starting character
  IdentifierInitialChars* = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz"
  IdentifierChars* = IdentifierInitialChars & "0123456789_"
  SpecialIdentifierChars* = "<>,:;!?@$%^&/*+-=~|({'" & '"'

type
  IntFormat = enum
    Dec, Hex, Bin

  ParserCombinationDefect* = object of Defect

  IndentType* = enum
    itUnset, itSpaces, itTabs

  Precedence* = distinct uint8

template `@`*(i: int): Precedence = Precedence(i)

# TODO: Replace operator precedence with hardcoded tables with precedence declaration in the language's stdlib
const
  UnaryPrecedence*: OrderedTable[string, Precedence] = {
    "@": @12,  # Iterator-like types to mutable lists
    "$": @10,  # `toString`
    "not": @6, # Boolean algebra and bitops
    "+": @0,
    "-": @0
  }.toOrderedTable

  InfixPrecedence*: OrderedTable[string, Precedence] = {
    ".": @11,  # Field access and custom dot operators
    "^": @10,  # Exponentiation
    "/": @9,
    "*": @9,
    "shl": @8,
    "shr": @8,
    "+": @8,
    "-": @8,
    "..<": @7, # Also for slicing, but it takes away 1 from the right hand side, useful for `0..<myArray.len`
    "..": @7,  # Range/slice operator, useful for getting slices of arrays or substrings
    "<": @6,
    ">": @6,
    "<=": @6,
    ">=": @6,
    "!=": @6,
    "==": @6,
    "in": @6,
    "notin": @6,
    "is": @6,
    "isnot": @6,
    "and": @5, # Boolean algebra and bitops
    "&": @5,   # String concatenation and typeclasses
    "xor": @4, # Boolean algebra and bitops
    "or": @4,  # Boolean algebra and bitops
    "|": @4,   # Purely for typeclasses
    "!": @3,   # For result types, `Ok!Error`, `Ok` being the type returned on success, `Error` being the type returned on failure
    "=": @2    # Assignment and assignment-like operators should have the lowest precedence
  }.toOrderedTable

  PostfixPrecedence*: OrderedTable[string, Precedence] = {
    "?": @3 # Used for `Option` types.
  }.toOrderedTable

type
  ParserState* = object
    nodes*: seq[AstNode]
    indentType: IndentType = itUnset
    indentStack, newlineLocs: seq[int]
    unaryPrecedence*, infixPrecedence*, postfixPrecedence*: OrderedTable[string, Precedence]

  ParserFailureCombinatorSide* = enum
    Left, Right

  ParseFailureSourceKind* = enum
    pfsIndentParser, pfsIdentifierParser, pfsAccQuoteParser, pfsStringParser, pfsNumberParser,
    pfsCharacterParser, pfsAndCombinator, pfsOrCombinator, pfsLinkCombinator, pfsMany1Combinator, pfsThen, pfsLabel

  ParseFailureSource* {.acyclic.} = object
    case kind*: ParseFailureSourceKind
    of {pfsIndentParser, pfsIdentifierParser, pfsAccQuoteParser, pfsStringParser, pfsNumberParser}:
      npLineInfo*: AstLineInfo
    of pfsCharacterParser:
      pLineInfo*: AstLineInfo
      pChar*: char
    of pfsAndCombinator:
      acSide*: ParserFailureCombinatorSide
      acFailure*: ref ParseFailureSource
    of pfsOrCombinator:
      ocLeft*: ref ParseFailureSource
      ocRight*: ref ParseFailureSource
    of pfsLinkCombinator:
      lcFailure*: ref ParseFailureSource
      lcIndex*: Natural
    of pfsMany1Combinator:
      m1Failure*: ref ParseFailureSource
    of pfsThen:
      tFailure*: ref ParseFailureSource
    of pfsLabel:
      lFailure*: ref ParseFailureSource
      lName*: string
      lMsg*: string

  IndentParserFailure* = distinct ParseFailureSource
  IdentifierParserFailure* = distinct ParseFailureSource
  AccQuoteParserFailure* = distinct ParseFailureSource
  StringParserFailure* = distinct ParseFailureSource
  NumberParserFailure* = distinct ParseFailureSource
  CharacterParserFailure* = distinct ParseFailureSource
  AndCombinatorFailure* = distinct ParseFailureSource
  OrCombinatorFailure* = distinct ParseFailureSource
  LinkCombinatorFailure* = distinct ParseFailureSource
  Many1CombinatorFailure* = distinct ParseFailureSource
  ThenBlockFailure* = distinct ParseFailureSource
  LabelledFailure* = distinct ParseFailureSource

  ParseFailureSourcesNoParam = IndentParserFailure | IdentifierParserFailure | AccQuoteParserFailure |
    StringParserFailure | NumberParserFailure
  CombinatorFailures = AndCombinatorFailure | OrCombinatorFailure | LinkCombinatorFailure | Many1CombinatorFailure
  MiscFailures = ThenBlockFailure | LabelledFailure
  ParseFailureSources* = ParseFailureSourcesNoParam | CharacterParserFailure | CombinatorFailures | MiscFailures

  ParseFailureKind* = enum
    Unknown, EndOfInput, ExpectedWhitespace, CannotDetectIndentationType, InconsistentIndentationSpacing,
    NotAnIdentifier, ExpectedChar

  ParseFailure* = object
    state: ParserState
    source*: ParseFailureSource
    position*: Natural

    case kind*: ParseFailureKind
    of {Unknown, EndOfInput, ExpectedWhitespace, CannotDetectIndentationType, InconsistentIndentationSpacing}:
      discard
    of NotAnIdentifier:
      naiChar*: char
    of ExpectedChar:
      ecFoundChar*, ecExpectedChar*: char

  ParseSuccess* = object
    state: ParserState
    position*: Natural
    nodes*: seq[AstNodeIndex]

  ParseResult* = Result[ParseSuccess, ParseFailure]

  Parser* = proc(state: sink ParserState, input: openArray[char], position: Natural): ParseResult {.noSideEffect, gcsafe.}

# Useful converters
converter toFailureSource*(s: ParseFailureSources): ParseFailureSource = ParseFailureSource(s)
converter toFailureSource*(s: ref ParseFailureSources): ref ParseFailureSource = (ref ParseFailureSource)(s)

# Utility procs
template toKind(s: typedesc[ParseFailureSources]): ParseFailureSourceKind =
  when s is IndentParserFailure: pfsIndentParser
  elif s is IdentifierParserFailure: pfsIdentifierParser
  elif s is AccQuoteParserFailure: pfsAccQuoteParser
  elif s is StringParserFailure: pfsStringParser
  elif s is NumberParserFailure: pfsNumberParser
  elif s is CharacterParserFailure: pfsCharacterParser
  elif s is AndCombinatorFailure: pfsAndCombinator
  elif s is OrCombinatorFailure: pfsOrCombinator
  elif s is LinkCombinatorFailure: pfsLinkCombinator
  elif s is Many1CombinatorFailure: pfsMany1Combinator
  elif s is ThenBlockFailure: pfsThen
  elif s is LabelledFailure: pfsLabel
  else: {.error: "Unreachable `toKind` for `" & $n & "`.".}

proc add(state: var ParserState, node: sink AstNode): AstNodeIndex =
  state.nodes.add(node)
  ~state.nodes.high


proc toRef[T: ParseFailureSources | ParseFailureSource](src: sink T): ref T =
  result = new T
  result[] = src


func lineInfo*(failure: ParseFailureSource): AstLineInfo =
  ## Returns the line info of the `ParseFailureSource`, will iterate through the nodes until it finds the line info.
  var src = failure.toRef

  while true:
    if src == nil:
      raise newException(ValueError, "No lineinfo found!")

    case (src[]).kind
    of {pfsIndentParser, pfsIdentifierParser, pfsAccQuoteParser, pfsStringParser, pfsNumberParser}:
      result = failure.npLineInfo
      break
    of pfsCharacterParser:
      result = src.pLineInfo
      break
    of pfsAndCombinator:
      src = src.acFailure
    of pfsOrCombinator:
      src = src.ocLeft
    of pfsLinkCombinator:
      src = src.lcFailure
    of pfsMany1Combinator:
      src = src.m1Failure
    of pfsThen:
      src = src.tFailure
    of pfsLabel:
      src = src.lFailure


func lineInfoAt*(state: ParserState, position: Natural): AstLineInfo =
  ## Returns the line info using the given `position`, searching through `state.newlineLocs`,
  ## requires calling `newlineLocator` first if multiple lines are given.
  result = (0, 0, Opt.none(string))

  if state.newlineLocs.len == 0:
    # Returns the first line always, since we can prove there are no newlines in the state.
    result.line = 1
    result.column = position + 1
    return

  # `newlineLocs` stores the positions of all newlines in the file, so we use this to find the appropriate line
  for i in state.newlineLocs:
    if i >= position: break
    inc result.line

  # Lines are offset by 1
  inc result.line

  # Columns are offset by 1, also we determine the column by using the found line as the index
  result.column = (position - state.newlineLocs[result.line - 1] + 1)


func stringify(input: openArray[char]): string =
  ## Helper function that converts `openArray[char]` to a `string`
  result = newStringOfCap(input.len)
  for i in input: result.add i


func `$`*(src: ref ParseFailureSource): string = (if src == nil: "nil" else: $src[])
func `$`*(p: Precedence): string = '@' & $uint8(p)

# `init` procs
proc init*(T: typedesc[ParseFailureSourcesNoParam], lineInfo: AstLineInfo): T =
  ## Creates a new `ParseFailureSource` that has no params
  T(ParseFailureSource(npLineInfo: lineInfo, kind: T.toKind))

proc init*(T: typedesc[CharacterParserFailure], pChar: char, lineInfo: AstLineInfo): T =
  ## Creates a new `CharacterParserFailure`
  T(ParseFailureSource(pLineInfo: lineInfo, kind: pfsCharacterParser, pChar: pChar))

proc init*(T: typedesc[AndCombinatorFailure], side: ParserFailureCombinatorSide, src: ParseFailureSource): T =
  ## Creates a new `AndCombinatorFailure`
  T(ParseFailureSource(kind: pfsAndCombinator, acSide: side, acFailure: src.toRef))

proc init*(T: typedesc[OrCombinatorFailure], left, right: ParseFailureSource): T =
  ## Creates a new `OrCombinatorFailure`
  T(ParseFailureSource(kind: pfsOrCombinator, ocLeft: left.toRef, ocRight: right.toRef))

proc init*(T: typedesc[LinkCombinatorFailure], src: ParseFailureSource, idx: Natural): T =
  ## Creates a new `LinkCombinatorFailure`
  T(ParseFailureSource(kind: pfsLinkCombinator, lcFailure: src.toRef, lcIndex: idx))

proc init*(T: typedesc[Many1CombinatorFailure], src: ParseFailureSource): T =
  ## Creates a new `Many1CombinatorFailure`
  T(ParseFailureSource(kind: pfsMany1Combinator, m1Failure: src.toRef))

proc init*(T: typedesc[ThenBlockFailure], src: ParseFailureSource): T =
  ## Creates a new `ThenBlockFailure`
  T(ParseFailureSource(kind: pfsThen, tFailure: src.toRef))

proc init*(T: typedesc[LabelledFailure], src: ParseFailureSource, label: string, msg: string): T =
  ## Creates a new `LabelledFailure`
  T(ParseFailureSource(kind: pfsLabel, lFailure: src.toRef, lName: label, lMsg: msg))

proc init*(T: typedesc[ParserState]): T = T(indentStack: @[0], unaryPrecedence: UnaryPrecedence,
  infixPrecedence: InfixPrecedence, postfixPrecedence: PostfixPrecedence)


func newlineLocator*(state: sink ParserState, input: openArray[char], position: Natural): ParseResult =
  ## Finds all the newlines and adds them to the state, then returns the `position` as is.
  var
    offset = position
    currentState = state

  while offset < input.len:
    if input[offset] == '\n':
      currentState.newlineLocs.add offset

    inc offset

  ok(ParseSuccess(state: currentState, position: position, nodes: @[]))


func parseIndent*(state: sink ParserState, input: openArray[char], position: Natural): ParseResult =
  ## Parses an indent, input at offset must start with a newline.
  ## It'll only parse spaces or tabs, once it detects one or the other.
  var
    offset = position
    counter = 0
    currentState = state

  while input.len > offset:
    if input[offset] != '\n': return err(ParseFailure(state: currentState,
      source: IndentParserFailure.init(currentState.lineInfoAt(position)), kind: ExpectedChar, ecFoundChar: input[offset],
      ecExpectedChar: '\n'))
    while input.len > offset and input[offset] == '\n':
      inc offset

    if offset >= input.len:
      if currentState.indentStack.len != 1:
        currentState.indentStack.setLen(1)
        let idx = currentState.add Dedent.init(currentState.indentStack[0], currentState.lineInfoAt(position))
        return ok(ParseSuccess(state: currentState, position: offset, nodes: @[idx]))
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
      return err(ParseFailure(state: currentState, source: IndentParserFailure.init(currentState.lineInfoAt(position)),
      kind: CannotDetectIndentationType))

    while input.len >= offset and input[offset] in {' ', '\t'}:
      if input[offset] == ichar:
        inc counter
        inc offset
      else:
        return err(ParseFailure(state: currentState, source: IndentParserFailure.init(currentState.lineInfoAt(position)),
          kind: ExpectedChar, ecFoundChar: input[offset], ecExpectedChar: ichar))

    if input[offset] == '\n':
      counter = 0
      continue

    let lastISLen = currentState.indentStack.len

    if counter > currentState.indentStack[^1]:
      currentState.indentStack.add counter
    elif counter < currentState.indentStack[^1]:
      let i = currentState.indentStack.find(counter)
      if i == -1:
        return err(ParseFailure(state: currentState, source: IndentParserFailure.init(currentState.lineInfoAt(position)),
          kind: InconsistentIndentationSpacing))
      while counter < currentState.indentStack[^1]:
        if currentState.indentStack.len == 1:
          break
        currentState.indentStack.delete currentState.indentStack.len - 1
    else:
      return err(ParseFailure(state: currentState, source: IndentParserFailure.init(currentState.lineInfoAt(position)),
        kind: InconsistentIndentationSpacing))


    var
      node: AstNodeIndex = if currentState.indentStack.len > lastISLen:
        currentState.add Indent.init(currentState.indentStack.len, currentState.lineInfoAt(position))
      elif currentState.indentStack.len < lastISLen:
        currentState.add Dedent.init(currentState.indentStack.len, currentState.lineInfoAt(position))
      else:
        return ok(ParseSuccess(state: currentState, position: offset, nodes: @[]))

    return ok(ParseSuccess(state: currentState, position: offset, nodes: @[node]))


func parseIdentifier*(state: sink ParserState, input: openArray[char], position: Natural): ParseResult =
  ## Parses an identifier from `input` starting at `position`
  var
    offset = position
    currentState = state

  if offset >= input.len: return err(ParseFailure(state: currentState,
    source: IdentifierParserFailure.init(currentState.lineInfoAt(position)), kind: EndOfInput, position: offset))
  elif input[offset] notin IdentifierInitialChars & SpecialIdentifierChars:
    return err(ParseFailure(state: currentState, source: IdentifierParserFailure.init(currentState.lineInfoAt(position)),
      kind: NotAnIdentifier,
      naiChar: input[offset], position: offset))

  if input[offset] in SpecialIdentifierChars:
    while offset < input.len and input[offset] in SpecialIdentifierChars: inc offset
  else:
    while offset < input.len and input[offset] in IdentifierChars: inc offset

  if offset < input.len:
    if input[offset] notin "\n\t " & SpecialIdentifierChars & IdentifierChars:
      return err(ParseFailure(state: currentState, source: IdentifierParserFailure.init(currentState.lineInfoAt(position)),
        kind: ExpectedWhitespace, position: offset))

  let idx = currentState.add Identifier.init(input[position..<offset].stringify, currentState.lineInfoAt(position))
  ok(ParseSuccess(state: currentState, position: offset, nodes: @[idx]))


func parseAccQuote*(state: sink ParserState, input: openArray[char], position: Natural): ParseResult =
  ## Parses an accQuote from `input` starting at `position`
  var
    offset = position
    currentState = state

  if offset >= input.len: return err(ParseFailure(state: currentState, kind: EndOfInput, position: offset))

  if input[offset] != '`':
    return err(ParseFailure(state: currentState, source: AccQuoteParserFailure.init(currentState.lineInfoAt(position)),
      kind: ExpectedChar, ecFoundChar: input[offset], ecExpectedChar: '`', position: offset))
  inc offset

  while offset < input.len and input[offset] != '`':
    if input[offset] == '\n':
      return err(ParseFailure(state: currentState, source: AccQuoteParserFailure.init(currentState.lineInfoAt(position)),
        kind: ExpectedChar, ecFoundChar: input[offset], ecExpectedChar: '`', position: offset))
    inc offset

  if offset >= input.len: return err(ParseFailure(state: currentState,
    source: AccQuoteParserFailure.init(currentState.lineInfoAt(position)), kind: EndOfInput, position: offset))

  if input[offset] != '`':
    return err(ParseFailure(state: currentState, source: AccQuoteParserFailure.init(currentState.lineInfoAt(position)),
      kind: ExpectedChar, ecFoundChar: input[offset], ecExpectedChar: '`', position: offset))
  inc offset

  let
    ident = currentState.add Identifier.init(input[(position + 1)..<(offset - 1)].stringify, currentState.lineInfoAt(position + 1))
    accQuote = currentState.add AccQuote.init(ident, currentState.lineInfoAt(position))

  ok(ParseSuccess(state: currentState, position: offset, nodes: @[accQuote]))


func parseString*(state: sink ParserState, input: openArray[char], position: Natural): ParseResult =
  ## Parses a string from `input` starting at `position`
  var
    offset = position
    currentState = state

  if offset >= input.len: return err(ParseFailure(state: currentState,
    source: StringParserFailure.init(currentState.lineInfoAt(position)), kind: EndOfInput, position: offset))

  if input[offset] != '"':
    return err(ParseFailure(state: currentState, source: StringParserFailure.init(currentState.lineInfoAt(position)),
      kind: ExpectedChar, ecFoundChar: input[offset], ecExpectedChar: '"', position: offset))
  inc offset

  while offset < input.len and input[offset] != '"':
    if input[offset] == '\n':
      return err(ParseFailure(state: currentState, source: StringParserFailure.init(currentState.lineInfoAt(position)),
        kind: ExpectedChar, ecFoundChar: input[offset], ecExpectedChar: '`', position: offset))
    inc offset

  if offset >= input.len: return err(ParseFailure(state: currentState,
    source: StringParserFailure.init(currentState.lineInfoAt(position)), kind: EndOfInput, position: offset))

  if input[offset] != '"':
    return err(ParseFailure(state: currentState, source: StringParserFailure.init(currentState.lineInfoAt(position)),
      kind: ExpectedChar, ecFoundChar: input[offset], ecExpectedChar: '"', position: offset))
  inc offset
  if offset < input.len:
    inc offset
    if input[offset] notin "\n\t " & SpecialIdentifierChars:
      return err(ParseFailure(state: currentState, source: StringParserFailure.init(currentState.lineInfoAt(position)),
        kind: ExpectedWhitespace, position: offset))

  let idx = currentState.add String.init(input[(position + 1)..<(offset - 1)].stringify,
    currentState.lineInfoAt(position))

  ok(ParseSuccess(state: currentState, position: offset, nodes: @[idx]))


func parseNumber*(state: sink ParserState, input: openArray[char], position: Natural): ParseResult =
  ## Parses a number from `input` starting at `position`
  ##
  ## TODO: Handle overflow
  var
    offset = position
    currentState = state
    numFormat = Dec
    isFloat = false

  if offset > input.len: return err(ParseFailure(state: currentState,
    source: NumberParserFailure.init(currentState.lineInfoAt(position)), kind: EndOfInput, position: offset))

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

  if offset < input.len:
    if input[offset] notin "\n\t " & SpecialIdentifierChars:
      return err(ParseFailure(state: currentState, source: NumberParserFailure.init(currentState.lineInfoAt(position)),
        kind: ExpectedWhitespace, position: offset))

  let idx = if isFloat:
    currentState.add Float.init(floatVal, currentState.lineInfoAt(position))
  else:
    currentState.add Int.init(intVal, currentState.lineInfoAt(position))

  ok(ParseSuccess(state: currentState, position: offset, nodes: @[idx]))


proc charParser*(c: char): Parser =
  ## Creates a parser that parses a single character `c`
  func parseCharacter(state: sink ParserState, input: openArray[char], position: Natural): ParseResult =
    ## Creates a parser that parses a single character `c`
    var currentState = state
    if position >= input.len: return err(ParseFailure(state: currentState,
      source: CharacterParserFailure.init(c, currentState.lineInfoAt(position)), kind: EndOfInput, position: position))
    elif input[position] == c: ok(ParseSuccess(state: currentState, position: position + 1, nodes: @[]))
    else: err(ParseFailure(state: currentState, source: CharacterParserFailure.init(c, currentState.lineInfoAt(position)),
      kind: ExpectedChar, ecExpectedChar: c, ecFoundChar: input[position], position: position))

  parseCharacter


macro `and`*(a, b: Parser): Parser =
  ## Combines parsers `a` and `b`, running `b` only if `a` succeeds.
  genAst(a, b, andCombinator=genSym(nskProc, "andCombinator"), result=ident("result")) do:
    func andCombinator(state: sink ParserState, input: openArray[char], position: Natural): ParseResult =
      ## Runs parsers `a` and `b`, running `b` only if `a` succeeds.
      result = a(state, input, position)
      if result.isErr:
        result.unsafeError().source = AndCombinatorFailure.init(Left, result.unsafeError().source)
        return

      let nodes = result.unsafeGet().nodes
      result = b(result.unsafeGet().state, input, result.unsafeGet().position)

      if result.isErr:
        result.unsafeError().source = AndCombinatorFailure.init(Right, result.unsafeError().source)
        return

      result.unsafeGet().nodes = nodes & result.unsafeGet().nodes
      

    andCombinator


macro `or`*(a, b: Parser): Parser =
  ## Combines parsers `a` and `b`, running `b` after `a` only if `a` fails.
  genAst(a, b, orCombinator=genSym(nskProc, "orCombinator"), result=ident("result")) do:
    func orCombinator(state: sink ParserState, input: openArray[char], position: Natural): ParseResult =
      ## Runs parsers `a` or `b`, running `b` only if `a` fails.
      result = a(state, input, position)
      if result.isErr:
        let srcA = result.unsafeError().source
        result = b(result.unsafeError().state, input, position)
        if result.isErr:
          result.unsafeError().source = OrCombinatorFailure.init(srcA, result.unsafeError().source)

    orCombinator


proc link*(ps: varargs[Parser]): Parser =
  ## Combines all parsers provided into one using the same logic as `and`.
  if ps.len < 2:
    raise newException(ParserCombinationDefect, "`link` requires at least two parsers to join together.")

  let parsers = @ps

  func linkCombinator(state: sink ParserState, input: openArray[char], position: Natural): ParseResult =
    ## Combines parsers `a` and `b`, running `b` only if `a` fails.
    var
      idx = 0

    result = parsers[0](state, input, position)

    for p in parsers[1..^1]:
      if result.isErr:
        let src = result.unsafeError().source
        result.unsafeError().source = LinkCombinatorFailure.init(src, idx)
        break
      inc idx
      let nodes = result.unsafeGet().nodes
      result = p(result.unsafeGet().state, input, result.unsafeGet().position)
      if result.isOk:
        result.unsafeGet().nodes = nodes & result.unsafeGet().nodes

  linkCombinator


macro then*(p: Parser, input: untyped{nkIdent}, body): Parser =
  ## Creates a parser that runs `p` and then any code passed to the `body`
  let thenBlock = genSym(nskProc, "thenBlock")

  # TODO: ThenBlockFailure?
  genAst(thenBlock, result=ident("result")) do:
    func thenBlock(state: sink ParserState, input: openArray[char], position: Natural): ParseResult =
      ## Runs the given parser, and on success then runs `body`.
      result = p(state, input, position)

      if result.isOk:
        body
        if result.isErr:
          result.unsafeError().source = ThenBlockFailure.init(result.unsafeError().source)

    thenBlock


macro label*(p: Parser, name: static string, msg: string): Parser =
  ## Creates a parser that creates a function with the specified `name` and then executes `p`, uses `msg` as the
  ## error message if `p` fails.
  let functionName = genSym(nskProc, name)

  genAst(p, msg, functionName, result=ident("result"), name=($name)) do:
    func functionName(state: sink ParserState, input: openArray[char], position: Natural): ParseResult =
      ## A labelled parser that runs the given parser.
      result = p(state, input, position)
      if result.isErr:
        result.unsafeError().source = LabelledFailure.init(result.unsafeError().source, name, msg.replace(
          "{lineInfo}",
          pretty(result.unsafeError().source.lineInfo)
        ))

    functionName

template label*(p: Parser, name: static string): Parser =
  ## Creates a parser that creates a function with the specified `name` and then executes `p`
  label(p, name, "<`" & name & "` wasn't provided with an error message.>")

proc lift*(p: ParseResult): Parser =
  ## Generates a parser that returns the given `ParseResult`, lifting it.
  # TODO: ThenBlockFailure?
  func liftCombinator(state: sink ParserState, input: openArray[char], position: Natural): ParseResult =
    ## Returns the given `ParseResult`, lifting it.
    if p.isOk:
      ok(ParseSuccess(state: state, position: position, nodes: p.unsafeGet().nodes))
    else:
      var res = p
      res.unsafeError().state = state
      res

  liftCombinator


proc optional*(p: Parser): Parser =
  ## Creates a parser that runs `p` exactly once, but still returns okay if the parsing fails
  func optionalCombinator(state: sink ParserState, input: openArray[char], position: Natural): ParseResult =
    ## Runs `p` exactly once, but still returns okay if the parsing fails
    result = p(state, input, position)
    if result.isErr:
      result = ok(ParseSuccess(state: result.unsafeError().state, position: position, nodes: @[]))

  optionalCombinator


proc many0*(p: Parser): Parser =
  ## Creates a parser that runs `p` 0 or more times
  func many0Combinator(state: sink ParserState, input: openArray[char], position: Natural): ParseResult =
    ## Runs `p` 0 or more times
    result = ok(ParseSuccess(state: state, position: position, nodes: @[]))

    while result.isOk:
      let res = p(result.unsafeGet().state, input, result.unsafeGet().position)
      if res.isErr: break
      let nodes = result.unsafeGet().nodes
      result = res
      result.unsafeGet().nodes = nodes & result.unsafeGet().nodes

  many0Combinator


proc many1*(p: Parser): Parser =
  ## Creates a parser that runs `p` 1 or more times
  func many1Combinator(state: sink ParserState, input: openArray[char], position: Natural): ParseResult =
    ## Runs `p` 1 or more times
    result = p(state, input, position)

    if result.isErr:
      result.unsafeError().source = Many1CombinatorFailure.init(result.unsafeError().source)

    while result.isOk:
      let res = p(result.unsafeGet().state, input, result.unsafeGet().position)
      if res.isErr: break
      let nodes = result.unsafeGet().nodes
      result = res
      result.unsafeGet().nodes = nodes & result.unsafeGet().nodes

  many1Combinator

const skipWhitespace* = many0(charParser(' ')).label("skipWhitespace")