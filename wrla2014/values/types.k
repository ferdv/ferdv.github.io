// Types of values

require "../funcons/expr.k"

module TYPES
  imports EXPR

  syntax Types ::= "no-type"
  
  syntax Expr ::= "typed" "(" Expr "," Expr ")"         [strict]
                | "null-value" "(" Expr ")"             [strict]
  
  syntax Values ::= Types
endmodule
