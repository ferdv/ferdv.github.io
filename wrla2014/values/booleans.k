// Boolean values

require "../funcons/expr.k"
require "../values/types.k"

module BOOLEANS
  imports EXPR
  imports TYPES
  
  syntax Expr ::= "not" "(" Expr ")"                            [strict]
                | "equal" "(" Expr "," Expr ")"                 [strict]

  syntax Types ::= "booleans"

  syntax Values ::= Bool
  
  rule not(true) => false
  rule not(false) => true
  rule equal(V1:Values, V2:Values) => V1 =K V2
  rule null-value(booleans) => false
  rule typed(B:Bool, booleans) => B
endmodule
