// Bindings

require "../funcons/decl.k"

module BINDINGS
  imports DECL

  syntax Bindings ::= bindings(Map)

  syntax Decl ::= "bindings-union" "(" Decl "," Decl ")"     [strict]

  rule bindings-union(bindings(M1:Map), bindings(M2:Map)) => bindings(M1 M2)
endmodule
