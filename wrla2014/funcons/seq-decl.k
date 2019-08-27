// Sequencing command before declaration

require "../funcons/comm.k"
require "../funcons/decl.k"

module SEQ-DECL
  imports COMM
  imports DECL

  syntax Decl ::= "seq" "(" Comm "," Decl ")"       [strict(1)]

  rule seq(skip, D:Decl) => D
endmodule
