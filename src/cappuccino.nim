import pkg/results

import ./cappuccino/[combinator, nodes]

const parser = parseNumber and skipWhitespace and parseAccQuote
let res = parser(ParserState(), "0b110 `a_cd`")

echo res