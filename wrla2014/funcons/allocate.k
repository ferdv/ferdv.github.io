// Variable allocation

require "../funcons/expr.k"
require "../values/types.k"
require "../values/values.k"
require "../values/variables.k"

module ALLOCATE
  imports EXPR
  imports TYPES
  imports VALUES
  imports VARIABLES

  syntax Expr ::= "allocate" "(" Expr ")"       [strict]

  rule <k> allocate(variables(T:Types)) => V ...</k>
       <store>... .Map => V |-> no-value ...</store>  when fresh(V:Variables)
endmodule
