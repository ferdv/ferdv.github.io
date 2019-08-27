// Sequencing commands

require "../funcons/comm.k"

module SEQ-COMM
  imports COMM

  syntax Comm ::= "seq" "(" Comm "," Comm ")"       [strict(1)]

  rule seq(skip, C:Comm) => C
endmodule
