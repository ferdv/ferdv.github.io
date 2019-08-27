// Variable value expressions

require "../funcons/expr.k"
require "../values/values.k"
require "../values/variables.k"

module STORED-VALUE
  imports EXPR
  imports VALUES
  imports VARIABLES

  syntax Expr ::= "stored-value" "(" Expr ")"       [strict]

  rule <k> stored-value(Var:Variables) => V:Values ...</k>
       <store>... Var |-> V ...</store>
endmodule
