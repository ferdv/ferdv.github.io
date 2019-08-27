// Identifier bound-value expressions

require "../funcons/expr.k"
require "../values/values.k"

module BOUND-VALUE
  imports EXPR
  imports VALUES

  syntax Expr ::= "bound-value" "(" Id ")"

  rule <k> bound-value(I:Id) => V:Values ...</k>
       <env>... I |-> V ...</env>
endmodule
