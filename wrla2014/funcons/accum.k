// Sequencing command before declaration

require "../funcons/decl.k"
require "../funcons/reset-env.k"
require "../values/bindings.k"

module ACCUM
  imports DECL
  imports RESET-ENV
  imports BINDINGS

  syntax Decl ::= "accum" "(" Decl "," Decl ")"     [strict(1)]

  rule <k> accum(bindings(Env:Map), D:Decl) =>
             reset-env(Env', bindings-union(bindings(Env), D)) ...</k>
       <env> Env':Map => Env'[Env] </env>
endmodule
