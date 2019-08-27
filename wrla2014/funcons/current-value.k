// Stored value of a variable, identity on other values

require "../funcons/expr.k"
require "../funcons/stored-value.k"
require "../values/values.k"
require "../values/variables.k"

module CURRENT-VALUE
  imports EXPR
  imports STORED-VALUE
  imports VALUES
  imports VARIABLES

  syntax Expr ::= "current-value" "(" Expr ")"      [strict]

  rule current-value(V:Variables) => stored-value(V)

  rule current-value(V:Values) => V [owise] // when ~ V:Variables
endmodule
