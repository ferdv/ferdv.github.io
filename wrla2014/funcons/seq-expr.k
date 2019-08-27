// Sequencing command before expression

require "../funcons/comm.k"
require "../funcons/expr.k"

module SEQ-EXPR
  imports COMM
  imports EXPR

  syntax Expr ::= "seq" "(" Comm "," Expr ")"		[strict(1)]

  rule seq(skip, E:Expr) => E
endmodule
