// Matching patterns to values

require "../funcons/decl.k"
require "../funcons/expr.k"
require "../funcons/match.k"
require "../funcons/supply-decl.k"
require "../values/patterns.k"
require "../values/values.k"

module MATCH
  imports DECL
  imports EXPR
  imports MATCH
  imports SUPPLY-DECL
  imports PATTERNS
  imports VALUES

  syntax Decl ::= "match" "(" Patterns "," Expr ")"     [strict(2)]

  rule match(abstraction(D:Decl), V:Values) => supply(V, D)
endmodule
