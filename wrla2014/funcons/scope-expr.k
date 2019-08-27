// Scoping declaration in expression

require "../funcons/comm.k"
require "../funcons/decl.k"
require "../funcons/reset-env.k"
require "../values/bindings.k"

module SCOPE-EXPR
  imports EXPR
  imports DECL
  imports RESET-ENV
  imports BINDINGS

  syntax Expr ::= "scope" "(" Decl "," Expr ")"		[strict(1)]

  rule <k> scope(bindings(Env:Map), E:Expr) => reset-env(Env', E) ...</k>
       <env> Env':Map => Env'[Env] </env>
endmodule
