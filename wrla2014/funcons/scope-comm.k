// Scoping declarations in command

require "../funcons/comm.k"
require "../funcons/decl.k"
require "../funcons/reset-env.k"
require "../values/bindings.k"

module SCOPE-COMM
  imports COMM
  imports DECL
  imports RESET-ENV
  imports BINDINGS

  syntax Comm ::= "scope" "(" Decl "," Comm ")"     [strict(1)]

  rule <k> scope(bindings(Env:Map), C:Comm) => reset-env(Env', C) ...</k>
       <env> Env':Map => Env'[Env] </env>
endmodule
