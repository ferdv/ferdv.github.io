// Expression funcons

require "../values/values.k"

module EXPR
  imports VALUES
  
  syntax Expr ::= Values
  syntax KResult ::= Values
endmodule
