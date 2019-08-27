// Supply value to declaration

require "../funcons/expr.k"
require "../funcons/decl.k"
require "../funcons/reset-given.k"
require "../values/values.k"

module SUPPLY-DECL
  imports DECL
  imports EXPR
  imports RESET-GIVEN
  imports VALUES

  syntax Decl ::= "supply" "(" Expr "," Decl ")"              [strict(1)]

  rule <k> supply(V:Values, D:Decl) => reset-given(V', D) ...</k>
       <given> V' => V </given>
 endmodule
