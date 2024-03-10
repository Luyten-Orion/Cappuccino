import std/[
  strformat,
  strutils
]

import ./types

type
  AstPrinter* = ref object of Visitor[string]
    depth*: int = 0

template T*(t: typedesc[AstPrinter] | AstPrinter): typedesc[string] = string

template indent(v: AstPrinter): string = repeat("  ", v.depth)

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
  result &= v.indent & "Expression: " & node.expression.accept(v) & "\n"
  dec v.depth

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