// Assignment commands

require "../funcons/comm.k"
require "../funcons/expr.k"
require "../values/values.k"
require "../values/variables.k"

module ASSIGN
  imports COMM
  imports EXPR
  imports VALUES
  imports VARIABLES

  syntax Comm ::= "assign" "(" Expr "," Expr ")"        [strict]

  rule <k> assign(Var:Variables, V:Values) => skip ...</k>
       <store>... Var |-> ( _:Values => V ) ...</store>
endmodule
