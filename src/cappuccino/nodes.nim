import std/sequtils

type
  AstNodeKind* = enum
    ankIdentifier, ankAccQuote, ankString, ankInt, ankFloat, ankCall, ankInfix, ankPrefix,
    ankIndent, ankDedent

const ankCallKinds* = {ankCall, ankInfix, ankPrefix}

type
  AstBuilderDefect* = object of Defect

  AstNodeIndex* = distinct int

  AstNode* = object
    case kind*: AstNodeKind
    of {ankIdentifier, ankAccQuote, ankString}:
      strVal*: string

    of ankInt:
      intVal*: int64

    of ankFloat:
      floatVal*: float64

    of ankCallKinds:
      caller*: AstNodeIndex
      callees*: seq[AstNodeIndex]
    
    of ankIndent, ankDedent:
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
  when n is Identifier: ankIdentifier
  elif n is AccQuote: ankAccQuote
  elif n is String: ankString
  elif n is Int: ankInt
  elif n is Float: ankFloat
  elif n is Call: ankCall
  elif n is Infix: ankInfix
  elif n is Prefix: ankPrefix
  elif n is Indent: ankIndent
  elif n is Dedent: ankDedent
  else: {.error: "Unreachable `toKind` for `" & $n & "`.".}

# Converters to make life slightly easier
converter toAstNode*(n: AstNodes): AstNode = AstNode(n)
converter toAstNode*(ns: seq[AstNodes]): seq[AstNode] = ns.mapIt(AstNode(it))

# Initialisers for AST nodes, distinct nodes provide type safety and nicer syntax :)
proc init*(T: typedesc[Identifier | AccQuote | String], val: string): T =
  ## Initializes an `Identifier`, `AccQuote` or `String` from `val`
  T(AstNode(kind: T.toKind, strVal: val))

proc init*(_: typedesc[Int], val: SomeInteger): Int =
  ## Initializes an `Int` from `val`
  Int(AstNode(kind: ankInt, intVal: val))

proc init*(_: typedesc[Float], val: SomeFloat): Int =
  ## Initializes a `Float` from `val`
  Int(AstNode(kind: ankFloat, floatVal: val))

proc init*[T: AstCalls](_: typedesc[T], caller: AstNodeIndex, callees: varargs[AstNodeIndex]): T =
  ## Initializes any given `AstCall` from `caller` and `callees`, though `Prefix` must always have one callee
  when T is Prefix:
    if callees.len != 1:
      raise newException(AstBuilderDefect, "Prefix must have exactly one callee!")

  T(AstNode(kind: T.toKind, caller: caller, callees: callees.toSeq))

proc init*[T: AstIndents](_: typedesc[T], depth: int): T =
  ## Initializes any given `AstIndent` from `depth`
  T(AstNode(kind: T.toKind, depth: depth))