// Closure construction

require "../funcons/expr.k"
require "../funcons/closure.k"
require "../values/bindings.k"
require "../values/functions.k"

module CLOSE
  imports EXPR
  imports CLOSURE
  imports BINDINGS
  imports FUNCTIONS

  syntax Expr ::= "close" "(" Expr ")"      [strict]

  rule <k> close(abstraction(E:Expr)) =>
             abstraction(closure(E, bindings(Env))) ...</k>
       <env> Env:Map </env>
endmodule
