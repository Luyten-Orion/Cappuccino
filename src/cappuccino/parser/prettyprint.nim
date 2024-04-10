import std/[
  strformat,
  strutils
]

import ./[
  errors,
  types
]

type
  AstPrinter* = ref object of Visitor[string]
    depth*: int = 0

template T*(t: typedesc[AstPrinter] | AstPrinter): typedesc[string] = string

template indent(v: AstPrinter): string = repeat("  ", v.depth)

# Quick AST visiting (forward declares)
func accept*(node: Identifier, v: AstPrinter): string
func accept*(node: FunctionCall, v: AstPrinter): string
func accept*(node: Grouping, v: AstPrinter): string
func accept*(node: Literal, v: AstPrinter): string
func accept*(node: Expression, v: AstPrinter): string

func visitIdentifier*(v: AstPrinter, node: Identifier): string =
  result = &"`{node.name}`"

func visitFunctionCall*(v: AstPrinter, node: FunctionCall): string =
  result = v.indent & "FunctionCall(\n"
  inc v.depth
  result &= v.indent & "Caller: " & node.function.accept(v) & ",\n"
  result &= v.indent & "Arguments: [\n"
  inc v.depth
  for arg in node.arguments:
    result &= v.indent & arg.accept(v) & ",\n"
  
  result.setLen(result.len - 2)
  result &= "\n"
  dec v.depth
  result &= v.indent & "]\n"
  dec v.depth

func visitGrouping*(v: AstPrinter, node: Grouping): string =
  result = v.indent & "Grouping(\n"
  inc v.depth
  result &= v.indent & "Child: " & node.child.accept(v) & "\n"
  dec v.depth
  result &= v.indent & ")\n"

func visitLiteral*(v: AstPrinter, node: Literal): string =
  case node.litKind
    of String:
      result = &"StrLit "
      result.addQuoted(node.strVal)
    of Integer:
      result = &"IntLit {node.intVal}"
    of Float:
      result = &"FloatLit {node.floatVal}"
    of Boolean:
      result = &"BoolLit {node.boolVal}"
    of Nil:
      result = "NilLit"

func visit*(v: AstPrinter, node: Expression): string =
  if node == nil:
    raise newException(CappuccinoVisitingError, "Given node is nil!")

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
      raise newException(CappuccinoVisitingError, "Unimplemented visit call!")

  if result[^1] == '\n':
    result.setLen(result.len - 1)

func accept*(node: Identifier, v: AstPrinter): string = (if node != nil: v.visitIdentifier(node) else: "...")
func accept*(node: FunctionCall, v: AstPrinter): string = (if node != nil: v.visitFunctionCall(node) else: "...")
func accept*(node: Grouping, v: AstPrinter): string = (if node != nil: v.visitGrouping(node) else: "...")
func accept*(node: Literal, v: AstPrinter): string = (if node != nil: v.visitLiteral(node) else: "...")
func accept*(node: Expression, v: AstPrinter): string = (if node != nil: v.visit(node) else: "...")