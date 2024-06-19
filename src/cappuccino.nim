import std/[parseutils, strutils, sequtils, genasts, macros]

import pkg/[results]


const
  ## The set of characters that can be used as an identifier's starting character
  IdentifierInitialChars* = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz"
  IdentifierChars* = IdentifierInitialChars & "0123456789_"


type
  IntFormat = enum
    Dec, Hex, Bin

  ParseFailureKind* = enum
    Unknown, EndOfInput, ExpectedWhitespace, NotAnIdentifier, ExpectedChar

  ParseFailure* = object
    position*: Natural

    case kind*: ParseFailureKind
    of {Unknown, EndOfInput,ExpectedWhitespace}:
      discard
    of NotAnIdentifier:
      naiChar*: char
    of ExpectedChar:
      ecFoundChar*, ecExpectedChar*: char

  AstNodeKind* = enum
    ankIdentifier, ankString, ankInt, ankFloat
  
  AstNode* = ref object
    case kind*: AstNodeKind
    of {ankIdentifier, ankString}:
      strVal*: string
    of ankInt:
      intVal*: int64
    of ankFloat:
      floatVal*: float64

  Identifier* = distinct AstNode
  String* = distinct AstNode
  Int* = distinct AstNode
  Float* = distinct AstNode

  AstNodes* = Identifier | String | Int

  ParseSuccess* = object
    position*: Natural
    nodes*: seq[AstNode]

  ParseResult* = Result[ParseSuccess, ParseFailure]

  Parser* = proc(input: openArray[char], position: Natural = 0): ParseResult

# Converters to make life slightly easier
converter toAstNode*(n: AstNodes): AstNode = AstNode(n)
converter toAstNode*(ns: seq[AstNodes]): seq[AstNode] = ns.mapIt(AstNode(it))

# Initialisers for AST nodes, distinct nodes provide type safety and nicer syntax :)
proc init*(T: typedesc[Identifier | String], val: string): Identifier =
  ## Initializes an `Identifier` or `String` from `val`
  const kind = when T is Identifier: ankIdentifier else: ankString
  T(AstNode(kind: kind, strVal: val))

proc init*(_: typedesc[Int], val: SomeInteger): Int =
  ## Initializes an `Int` from `val`
  Int(AstNode(kind: ankInt, intVal: val))

proc init*(_: typedesc[Float], val: SomeFloat): Int =
  ## Initializes a `Float` from `val`
  Int(AstNode(kind: ankFloat, floatVal: val))

# Flatten proc
proc flatten(a, b: ParseSuccess): ParseSuccess =
  ## Flattens two `ParseSuccess` objects into one, requires copying, is there a better way without using a state?
  result = ParseSuccess(
    position: b.position,
    nodes: newSeqOfCap[AstNode](a.nodes.len + a.nodes.len)
  )

  result.nodes = a.nodes & b.nodes

# 'Call' just calls `p` and returns the result, flattening as needed
template call(result: ParseResult, p: Parser, input: openArray[char], errBody: typed): ParseResult =
  ## Calls `p` on `input` and returns the result, using `res` for the offset
  if result.isOk:
    let res = p(input, result.get().position)
    if res.isOk:
      ok(flatten(result.get(), res.get()))
    else:
      res
  else:
    errBody

template call(result: ParseResult, p: Parser, input: openArray[char]): ParseResult =
  ## Calls `p` on `input` and returns the result, using `res` for the offset
  call(result, p, input, result)


proc stringify(input: openArray[char]): string =
  ## Helper function that converts `openArray[char]` to a `string`
  result = newStringOfCap(input.len)
  for i in input: result.add i


proc parseIdentifier*(input: openArray[char], position: Natural = 0): ParseResult =
  ## Parses an identifier from `input` starting at `position`
  var offset = position

  if offset >= input.len: return err(ParseFailure(kind: EndOfInput, position: offset))
  elif input[offset] notin IdentifierInitialChars:
    return err(ParseFailure(kind: NotAnIdentifier, naiChar: input[offset], position: offset))

  while offset < input.len and input[offset] in IdentifierChars: inc offset

  ok(ParseSuccess(position: offset, nodes: @[Identifier.init(input[position..<offset].stringify)]))


proc parseNumber*(input: openArray[char], position: Natural = 0): ParseResult =
  ## Parses a number from `input` starting at `position`
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

  if offset < input.len and input[offset] notin Whitespace:
    return err(ParseFailure(kind: ExpectedWhitespace, position: offset))

  if isFloat:
    ok(ParseSuccess(position: offset, nodes: @[Float.init(floatVal)]))
  else:
    ok(ParseSuccess(position: offset, nodes: @[Int.init(intVal)]))


proc charParser*(c: char): Parser =
  ## Creates a parser that parses a single character `c`
  proc parseChar(input: openArray[char], position: Natural = 0): ParseResult =
    ## Creates a parser that parses a single character `c`
    if position >= input.len: return err(ParseFailure(kind: EndOfInput, position: position))
    elif input[position] == c: ok(ParseSuccess(position: position + 1, nodes: @[]))
    else: err(ParseFailure(kind: ExpectedChar, ecExpectedChar: c, ecFoundChar: input[position], position: position))

  parseChar

macro `and`*(a, b: Parser): Parser =
  ## Combines parsers `a` and `b`, running `b` only if `a` succeeds.
  let
    aName = a.getImpl().name().strVal
    bName = b.getImpl().name().strVal
    functionName = genSym(nskProc, "(" & aName & " and " & bName & ")")
    docComment = "Runs parsers `{a}` and `{b}`, running `{b}` only if `{a}` succeeds.".multiReplace(
      replacements={"{a}": aName, "{b}": bName}
    )
  genAst(functionName, docComment=newCommentStmtNode(docComment)) do:
    proc andCombinator(input: openArray[char], position: Natural = 0): ParseResult =
      docComment
      result = a(input, position)
      result = result.call(b, input)

    andCombinator


macro `or`*(a, b: Parser): Parser =
  ## Combines parsers `a` and `b`, running `b` after `a` only if `a` fails.
  let
    aName = a.getImpl().name().strVal
    bName = b.getImpl().name().strVal
    functionName = genSym(nskProc, "(" & aName & " and " & bName & ")")
    docComment = "Runs parser `{a}` or `{b}`, running `{b}` only if `{a}` fails.".multiReplace(
      replacements={"{a}": aName, "{b}": bName}
    )
  genAst(functionName) do:
    proc functionName(input: openArray[char], position: Natural = 0): ParseResult =
      docComment
      result = a(input, position)
      if result.isErr:
        result = b(input, position)
    
    functionName


proc isParserSignature(node: NimNode): bool =
  ## Returns `true` if `node` is a `NimNode` of `Parser`
  if node[3][0].kind notin {nnkIdent, nnkSym}:
    return false

  if eqIdent("Parser", node[3][0]):
    return true

  if not eqIdent("ParseResult", node[3][0]):
    return false

  if node[3][1][1].kind != nnkBracketExpr:
    return false

  if not eqIdent("openArray", node[3][1][1][0]) or not eqIdent("char", node[3][1][1][1]):
    return false

  if node[3][2].kind notin {nnkIdent, nnkSym} and not eqIdent("Natural", node[3][2][1]):
    return false

  true


proc resolveParser(node: NimNode): (NimNode, bool) =
  ## Resolves a `NimNode` of `Parser` into a `NimNode` for the symbol of `Parser`
  case node.kind
    of nnkSym:
      return (node, node.symKind == nskProc and node.getImpl().isParserSignature)
    of nnkCallKinds + {nnkConv}:
      for n in node:
        result = resolveParser(n)
        if result[1]:
          return
    else:
      discard


macro link*(parsers: varargs[Parser]): Parser =
  ## Combines all parsers provided into one using the same logic as `and`.
  var ps: seq[NimNode] # Symbols of parsers

  for p in parsers:
    let (node, ok) = resolveParser(p)
    if not ok: error("All parsers must be of type `Parser`!", node)
    ps.add node

  if parsers.len < 2: error("`link` requires at least two parsers!", parsers)

  let
    functionName = genSym(nskProc, "link(" & parsers.mapIt(it.repr).join(", ") & ")")
    docComment = "Combines the parsers {p} into one using the same logic as `and`.".replace(
      "{p}", '`' & parsers[0..(^2)].mapIt(it.repr).join("`, `") & '`' & " and `{p}`".replace(
        "{p}", parsers[^1].repr
      )
    )
    procBody = newStmtList()
    inputSym = genSym(nskParam, "input")
    positionSym = genSym(nskParam, "position")
    resSym = genSym(nskVar, "res")

  procBody.add newCommentStmtNode(docComment)

  procBody.add genAst(resSym, p=parsers[0], input=inputSym, position=positionSym) do:
    var resSym = p(input, position)

  for p in parsers[1..(^1)]:
    procBody.add genAst(resSym, p, input=inputSym, position=positionSym) do:
      resSym = resSym.call(p, input)

  procBody.add resSym
  
  result = newProc(functionName, @[
    bindSym("ParseResult"),
    newNimNode(nnkIdentDefs).add(inputSym, newNimNode(nnkBracketExpr).add(
      bindSym("openArray"), bindSym("char")
    ), newEmptyNode()),
    newNimNode(nnkIdentDefs).add(positionSym, newIdentNode("Natural"), newLit(0))
  ], procBody)

  result = newStmtList(result, functionName)


macro then*(p: Parser, body): Parser =
  ## Creates a parser that runs `p` and then any code passed to the `body`
  let
    pName = p.getImpl().name().strVal
    functionName=genSym(nskProc, "then")
    docComment = "Runs `p` and then any code passed to the `body`.".replace(
      "{p}", pName
    )
  genAst(functionName=genSym(nskProc, "then"), docComment=newCommentStmtNode(docComment)) do:
    proc functionName(input {.inject.}: openArray[char], position {.inject.}: Natural = 0): ParseResult =
      docComment
      result = p(input, position)
      body

    functionName


macro label*(p: Parser, name: untyped{nkIdent}): Parser =
  ## Creates a parser that creates a function with the specified `name` and then executes `p`
  let
    pName = p.getImpl().name().strVal
    functionName=genSym(nskProc, name.strVal)
    docComment = "The given parser `{p}` is labelled as `" & name.strVal & "`.".replace(
      "{p}", pName
    )

  genAst(functionName, docComment=newCommentStmtNode(docComment)) do:
    proc label(input: openArray[char], position: Natural = 0): ParseResult =
      docComment
      result = p(input, position)

    functionName

let res = link(
  Parser(parseNumber), charParser(' '), Parser(parseIdentifier)
)("0b110 a_cd")

echo res
if res.isOk:
  for i in res.get().nodes:
    echo i[]