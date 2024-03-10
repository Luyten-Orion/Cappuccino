import std/[
  streams,
  os
]

import ./[
  parser,
  lexer
]

import ./parser/[
  prettyprint
]

let fileName = paramStr(1)

var code: Stream

if fileName == "-":
  code = newFileStream(stdin)

elif fileName == "":
  raise OSError.newException("No file was provided to be interpreted!")

else:
  code = newFileStream(fileName, fmRead)

if code == nil:
  raise OSError.newException("The file could not be opened!")

var l = newLexer(code)

let tokens = l.lex()

#echo tokens

let p = newParser(tokens)
var pretty = AstPrinter()
var ast = p.parse()
echo ast == nil
echo pretty.visit(ast)