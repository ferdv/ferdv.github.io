// Tuple values

require "../funcons/expr.k"
require "../values/values.k"

module TUPLES
  imports EXPR
  imports VALUES
  
  syntax Tuples ::= "tuple" "(" List ")"
  
  syntax Expr ::= "tuple-prefix" "(" Expr "," Expr ")"			[strict]

  syntax Expr ::= "tuple-append" "(" Expr "," Expr ")"			[strict]

  syntax Values ::= Tuples
      
  rule tuple-prefix(V:Values, tuple(L:List)) => tuple(ListItem(V) L)
      
  rule tuple-append(tuple(L1:List), tuple(L2:List)) => tuple(L1 L2)
endmodule
