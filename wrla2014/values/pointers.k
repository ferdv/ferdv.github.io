// Pointers

require "../values/types.k"
require "../values/unit.k"
require "../values/values.k"
require "../values/variables.k"
require "../funcons/expr.k"

module POINTERS
  imports TYPES
  imports UNIT
  imports VALUES
  imports VARIABLES
  imports EXPR

  syntax Pointers ::= Unit | Variables
  
  syntax Expr ::= "pointers" "(" Expr ")"			[strict]
  
  syntax Types ::= "pointers" "(" Types ")"
  
  syntax Values ::= Pointers

  rule null-value(pointers(_:Types)) => null
  
  rule typed(P:Pointers, pointers(_:Types)) => P
endmodule
