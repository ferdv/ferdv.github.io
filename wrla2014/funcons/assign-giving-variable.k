// Assignment expressions giving variables

require "../funcons/expr.k"
require "../values/values.k"
require "../values/variables.k"

module ASSIGN-GIVING-VARIABLE
  imports EXPR
  imports VALUES
  imports VARIABLES

  syntax Expr ::= "assign-giving-variable" "(" Expr "," Expr ")"        [strict]

  rule <k> assign-giving-variable(Var:Variables, V:Values) => Var ...</k>
       <store>... Var |-> ( _ => V ) ...</store>
endmodule
