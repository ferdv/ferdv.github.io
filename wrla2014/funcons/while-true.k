// While-loop commands

require "../funcons/comm.k"
require "../funcons/expr.k"
require "../funcons/if-true-comm.k"
require "../funcons/seq-comm.k"

module WHILE-TRUE
  imports COMM
  imports EXPR
  imports IF-TRUE-COMM
  imports SEQ-COMM

  syntax Comm ::= "while-true" "(" Expr "," Comm ")"

  rule while-true(E:Expr, C:Comm) => 
          if-true(E, seq(C, while-true(E, C)), skip)
endmodule
