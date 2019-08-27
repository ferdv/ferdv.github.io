// Closure evaluation

require "../funcons/expr.k"
require "../funcons/reset-env.k"
require "../values/bindings.k"

module CLOSURE
  imports EXPR
  imports RESET-ENV
  imports BINDINGS

  syntax Expr ::= "closure" "(" Expr "," Bindings ")"

  rule <k> closure(E:Expr, bindings(Env:Map)) => reset-env(Env', E) ...</k>
       <env> Env':Map => Env </env>
endmodule
