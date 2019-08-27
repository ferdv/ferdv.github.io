// Conditional expressions

require "../funcons/expr.k"
require "../values/booleans.k"

module IF-TRUE-EXPR
  imports EXPR
  imports BOOLEANS

  syntax Expr ::= "if-true" "(" Expr "," Expr "," Expr ")"      [strict(1)]

  rule if-true(true, E:Expr, _) => E
  rule if-true(false, _, E:Expr) => E
endmodule
