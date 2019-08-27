// Supply value to command

require "../funcons/expr.k"
require "../funcons/reset-given.k"
require "../values/values.k"

module SUPPLY-EXPR
  imports EXPR
  imports RESET-GIVEN
  imports VALUES

  syntax Expr ::= "supply" "(" Expr "," Expr ")"              [strict(1)]

  rule <k> supply(V:Values, E:Expr) => reset-given(V', E) ...</k>
       <given> V' => V </given>
 endmodule
