// Abstraction application

require "../funcons/expr.k"
require "../funcons/supply-expr.k"
require "../values/functions.k"
require "../values/values.k"

module APPLY
  imports EXPR
  imports SUPPLY-EXPR
  imports FUNCTIONS
  imports VALUES

  syntax Expr ::= "apply" "(" Expr "," Expr ")"     [strict]

  rule apply(abstraction(E:Expr), V:Values) => supply(V, E)
endmodule
