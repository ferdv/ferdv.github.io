// Binding identifiers to values

require "../funcons/decl.k"
require "../funcons/expr.k"
require "../values/values.k"

module BIND-VALUE
  imports DECL
  imports EXPR
  imports VALUES

  syntax Decl ::= "bind-value" "(" Id "," Expr ")"      [strict(2)]

  rule bind-value(I:Id, V:Values) => bindings(I |-> V)
endmodule
