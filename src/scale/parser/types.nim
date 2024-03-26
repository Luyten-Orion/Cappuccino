import ../lexer

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
    child*: Expression

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