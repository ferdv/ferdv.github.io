// Input integer from stream

require "../funcons/expr.k"
require "../funcons/assign.k"
require "../funcons/seq-expr.k"
require "../values/streams.k"
require "../values/values.k"
require "../values/variables.k"

module INPUT
  imports EXPR
  imports ASSIGN
  imports SEQ-EXPR
  imports STREAMS
  imports VALUES
  imports VARIABLES

  syntax Expr ::= "input" "(" Expr "," Expr ")"				[strict]

  rule <k> input(standard-input, L:Variables) => 
             seq(assign(L, I:Int), standard-input) ...</k>
       <input> ListItem(I:Int) => . ... </input>
endmodule
