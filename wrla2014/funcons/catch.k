// Catching thrown values

require "../funcons/comm.k"
require "../funcons/expr.k"
require "../values/functions.k"
require "../values/values.k"
require "../values/xstacks.k"

module CATCH
  imports COMM
  imports EXPR
  imports FUNCTIONS
  imports VALUES
  imports XSTACKS

  syntax Expr ::= "catch" "(" Comm "," Expr ")"     [strict(2)]

  rule <k> (catch(C:Comm, F:Functions) => (C ~> popx)) ~> K </k>
       <xstack> . => (F, K, B) ...</xstack>
       <context> B:Bag </context>

  syntax K ::= "popx"

  rule <k> popx => . </k>
       <xstack> _:ListItem => . ...</xstack>
endmodule
