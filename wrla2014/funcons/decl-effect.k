// Commands as declarations

require "../funcons/decl.k"
require "../funcons/comm.k"
require "../values/bindings.k"


module DECL-EFFECT
  imports DECL
  imports COMM
  imports BINDINGS

  syntax Decl ::= "decl-effect" "(" Comm ")"        [strict]

  rule decl-effect(skip) => bindings(.)
endmodule
