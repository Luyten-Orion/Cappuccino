import std/sequtils

type
  AstNodeKind* = enum
    anIdentifier, anString, anAccQuote, anInt, anFloat, anCall, anInfix, anPrefix,
    anIndent, anDedent

const anCallKinds* = {anCall, anInfix, anPrefix}

type
  AstBuilderDefect* = object of Defect

  AstNodeIndex* = distinct int

  AstNode* = object
    case kind*: AstNodeKind
    of {anIdentifier, anString}:
      strVal*: string

    of anAccQuote:
      accQuote*: AstNodeIndex

    of anInt:
      intVal*: int64

    of anFloat:
      floatVal*: float64

    of anCallKinds:
      caller*: AstNodeIndex
      callees*: seq[AstNodeIndex]
    
    of anIndent, anDedent:
      depth*: int

  Identifier* = distinct AstNode
  AccQuote* = distinct AstNode
  String* = distinct AstNode
  Int* = distinct AstNode
  Float* = distinct AstNode
  Call* = distinct AstNode
  Infix* = distinct AstNode
  Prefix* = distinct AstNode
  Indent* = distinct AstNode
  Dedent* = distinct AstNode

  AstLiterals* = String | Int | Float
  AstCalls* = Call | Infix | Prefix
  AstIndents* = Indent | Dedent
  AstExprs* = Identifier | AccQuote | AstLiterals | AstCalls
  AstNodes* = AstExprs | AstIndents

# Utility procs
func `$`*(node: AstNodeIndex): string = '~' & (let n = int(node); if n < 0: '(' & $n & ')' else: $n)
func `~`*(n: int): AstNodeIndex = AstNodeIndex(n)
func `+`*(n: AstNodeIndex, offset: int): AstNodeIndex = AstNodeIndex(int(n) + offset)
func `-`*(n: AstNodeIndex, offset: int): AstNodeIndex = AstNodeIndex(int(n) - offset)
proc `+=`*(n: var AstNodeIndex, offset: int) = n = n + offset
proc `-=`*(n: var AstNodeIndex, offset: int) = n = n - offset

template toKind(n: typedesc[AstNodes]): AstNodeKind =
  when n is Identifier: anIdentifier
  elif n is AccQuote: anAccQuote
  elif n is String: anString
  elif n is Int: anInt
  elif n is Float: anFloat
  elif n is Call: anCall
  elif n is Infix: anInfix
  elif n is Prefix: anPrefix
  elif n is Indent: anIndent
  elif n is Dedent: anDedent
  else: {.error: "Unreachable `toKind` for `" & $n & "`.".}

# Converters to make life slightly easier
converter toAstNode*(n: AstNodes): AstNode = AstNode(n)
converter toAstNode*(ns: seq[AstNodes]): seq[AstNode] = ns.mapIt(AstNode(it))

# Initialisers for AST nodes, distinct nodes provide type safety and nicer syntax :)
proc init*(T: typedesc[Identifier | String], val: string): T =
  ## Initializes an `Identifier` or `String` from `val`
  T(AstNode(kind: T.toKind, strVal: val))

proc init*(T: typedesc[AccQuote], val: AstNodeIndex): T =
  ## Initializes an `AccQuote` from `val`
  T(AstNode(kind: T.toKind, accQuote: val))

proc init*(_: typedesc[Int], val: SomeInteger): Int =
  ## Initializes an `Int` from `val`
  Int(AstNode(kind: anInt, intVal: val))

proc init*(_: typedesc[Float], val: SomeFloat): Int =
  ## Initializes a `Float` from `val`
  Int(AstNode(kind: anFloat, floatVal: val))

proc init*(T: typedesc[AstCalls], caller: AstNodeIndex, callees: varargs[AstNodeIndex]): T =
  ## Initializes any given `AstCall` from `caller` and `callees`, though `Prefix` must always have one callee
  when T is Prefix:
    if callees.len != 1:
      raise newException(AstBuilderDefect, "Prefix must have exactly one callee!")

  T(AstNode(kind: T.toKind, caller: caller, callees: callees.toSeq))

proc init*[T: AstIndents](_: typedesc[T], depth: int): T =
  ## Initializes any given `AstIndent` from `depth`
  T(AstNode(kind: T.toKind, depth: depth))