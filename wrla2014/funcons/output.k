// Output to stream

require "../funcons/expr.k"
require "../values/streams.k"
require "../values/values.k"

module OUTPUT
  imports EXPR
  imports STREAMS
  imports VALUES

  syntax Expr ::= "output" "(" Expr "," Expr ")"				[strict]

  rule <k> output(standard-output, V:Values) => standard-output ...</k>
       <output> ... . => ListItem(V) </output>
endmodule
