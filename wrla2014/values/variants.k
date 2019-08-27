// Variant values

require "../funcons/expr.k"
require "../values/types.k"
require "../values/values.k"

module VARIANTS
  imports EXPR
  imports TYPES
  imports VALUES
  
  syntax Variants ::= "variant" "(" Id "," Values ")"
  
  syntax Expr ::= "variant" "(" Id "," Expr ")"			[strict(2)]
                | "original" "(" Id "," Expr ")"			[strict(2)]

  syntax Types ::= "variants"

  syntax Values ::= Variants
  
  rule original(I:Id, variant(I, V:Values)) => V
endmodule
