// Function abstractions

require "../funcons/expr.k"
require "../values/tuples.k"
require "../values/types.k"

module FUNCTIONS
  imports EXPR
  imports TUPLES
  imports TYPES

  syntax Functions ::= "abstraction" "(" Expr ")"

  syntax Expr ::= "functions" "(" Expr "," Expr ")"         [strict]

  syntax Types ::= "functions" "(" Tuples "," Types ")"

  syntax Values ::= Functions
endmodule
