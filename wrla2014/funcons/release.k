// Variable release

require "../funcons/comm.k"
require "../funcons/expr.k"
require "../values/values.k"
require "../values/variables.k"

module RELEASE
  imports COMM
  imports EXPR
  imports VALUES
  imports VARIABLES

  syntax Comm ::= "release" "(" Expr ")"		[strict]

  rule <k> release(V:Variables) => skip ...</k>
       <store>... V |-> _:Values => . ...</store>
endmodule
