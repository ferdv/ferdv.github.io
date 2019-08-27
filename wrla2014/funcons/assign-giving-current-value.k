// Assignment expressions giving current value

require "../funcons/expr.k"
require "../values/values.k"
require "../values/variables.k"

module ASSIGN-GIVING-CURRENT-VALUE
  imports EXPR
  imports VALUES
  imports VARIABLES

  syntax Expr ::= "assign-giving-current-value" "(" Expr "," Expr ")"		[strict]

  rule <k> assign-giving-current-value(Var:Variables, V':Values) => V ...</k>
       <store>... Var |-> ( V:Values => V' ) ...</store>
endmodule
