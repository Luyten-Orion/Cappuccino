import ../lexer
import ./errors

# Enums
type
  AstKind* = enum
    AkIdentifier, AkFunctionCall, AkGrouping, AkLiteral, AkNone

  LiteralKind* = enum
    String, Integer, Float, Boolean, Nil

# AST types
type
  Expression* = ref object of RootObj
    kind*: AstKind = AkNone

  Identifier* = ref object of Expression
    name*: string

  FunctionCall* = ref object of Expression
    function*: Expression
    arguments*: seq[Expression]

  Grouping* = ref object of Expression
    expression*: Expression

  Literal* = ref object of Expression
    case litKind*: LiteralKind
      of String:
        strVal*: string
      of Integer:
        intVal*: int
      of Float:
        floatVal*: float
      of Boolean:
        boolVal*: bool
      of Nil:
        discard

# Visitor-related code
type
  Visitor*[T] = ref object of RootObj

template visitErr(s: string) = raise newException(ZaphytVisitingError, "visit" & s & " is not implemented!")

# Displays a helpful error for unimplemented visitors
func visitIdentifier*[T](v: Visitor[T], node: Identifier) = visitErr("Identifier")
func visitFunctionCall*[T](v: Visitor[T], node: FunctionCall) = visitErr("FunctionCall")
func visitGrouping*[T](v: Visitor[T], node: Grouping) = visitErr("Grouping")
func visitLiteral*[T](v: Visitor[T], node: Literal) = visitErr("Literal")

# Generic visiting code so the 'accept' code from below works
func visitIdentifier*[V: Visitor](v: V, node: Identifier): V.T = v.visitIdentifier(node)
func visitFunctionCall*[V: Visitor](v: V, node: FunctionCall): V.T = v.visitFunctionCall(node)
func visitGrouping*[V: Visitor](v: V, node: Grouping): V.T = v.visitGrouping(node)
func visitLiteral*[V: Visitor](v: V, node: Literal): V.T = v.visitLiteral(node)

func visit*[V: Visitor](v: V, node: Expression): V.T =
  when V.T is void:
    template `result=`(_: auto) = discard

  result = case node.kind
    of AkIdentifier:
      v.visitIdentifier(node.Identifier)
    of AkFunctionCall:
      v.visitFunctionCall(node.FunctionCall)
    of AkGrouping:
      v.visitGrouping(node.Grouping)
    of AkLiteral:
      v.visitLiteral(node.Literal)
    else:
      raise newException(ZaphytVisitingError, "Unimplemented visit call!")

# Quick visiting for ASTs
func accept*[V: Visitor](node: Identifier, v: V): V.T = v.visitIdentifier(node)
func accept*[V: Visitor](node: FunctionCall, v: V): V.T = v.visitFunctionCall(node)
func accept*[V: Visitor](node: Grouping, v: V): V.T = v.visitGrouping(node)
func accept*[V: Visitor](node: Literal, v: V): V.T = v.visitLiteral(node)
func accept*[V: Visitor](node: Expression, v: V): V.T = v.visit(node)

# Parser
type
  Parser* = ref object
    tokens*: seq[Token]
    current*: int

func newParser*(tokens: seq[Token]): Parser = Parser(tokens: tokens)

func previous*(p: Parser): Token = p.tokens[p.current - 1]
func peek*(p: Parser): Token = p.tokens[p.current]
func atEnd*(p: Parser): bool = p.peek().typ == EOF

func check*(p: Parser, typ: TokenType): bool =
  if p.atEnd:
    return false
  else:
    return p.peek().typ == typ

func advance*(p: Parser) =
  if not p.atEnd:
    p.current += 1

func match*(p: Parser, types: set[TokenType]): bool =
  for typ in types:
    if p.check(typ):
      p.advance()
      return true

  return false