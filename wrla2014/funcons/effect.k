// Effects of expressions

require "../funcons/comm.k"

module EFFECT
  imports COMM

  syntax Comm ::= "effect" "(" K ")"        [strict]

  rule effect(_:KResult) => skip
endmodule
