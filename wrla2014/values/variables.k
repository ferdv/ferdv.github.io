// Variables

require "../values/types.k"
require "../values/values.k"
require "../funcons/expr.k"

module VARIABLES
  imports TYPES
  imports VALUES
  imports EXPR

  syntax Variables ::= "no-variable"

  syntax Expr ::= "variables" "(" Expr ")"          [strict]

  syntax Types ::= "variables" "(" Types ")"

  syntax Values ::= Variables

  rule typed(V:Variables, variables(_:Types)) => V
endmodule
