// Conditional commands

require "../funcons/comm.k"
require "../funcons/expr.k"
require "../values/booleans.k"

module IF-TRUE-COMM
  imports COMM
  imports EXPR
  imports BOOLEANS

  syntax Comm ::= "if-true" "(" Expr "," Comm "," Comm ")"		[strict(1)]

  rule if-true(true, C:Comm, _) => C
  rule if-true(false, _, C:Comm) => C
endmodule
