// Value given by supply

require "../funcons/expr.k"
require "../values/values.k"

module GIVEN
  imports EXPR
  imports VALUES

  syntax Expr ::= "given"

  rule <k> given => V:Values ...</k>
       <given> V </given>
endmodule
