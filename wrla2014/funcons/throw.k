// Throwing values

require "../funcons/comm.k"
require "../funcons/expr.k"
require "../funcons/apply.k"
require "../values/functions.k"
require "../values/values.k"
require "../values/xstacks.k"

module THROW
  imports COMM
  imports EXPR
  imports APPLY
  imports FUNCTIONS
  imports VALUES
  imports XSTACKS

  syntax Comm ::= "throw" "(" Expr ")"      [strict]

  rule <k> (throw(V':Values) ~> _) => (apply(F, V') ~> K) </k>
       <xstack> (F:Functions, K:K, B:Bag) => . ...</xstack>
       <context> _ => B </context>
endmodule
