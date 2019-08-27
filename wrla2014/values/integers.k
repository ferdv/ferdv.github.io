// Integer values

require "../funcons/expr.k"
require "../values/types.k"

module INTEGERS
  imports EXPR
  imports TYPES
  
  syntax Expr ::= "int-plus" "(" Expr "," Expr ")"              [strict]
                | "int-minus" "(" Expr "," Expr ")"             [strict]
                | "int-times" "(" Expr "," Expr ")"             [strict]
                | "int-div" "(" Expr "," Expr ")"               [strict]
                | "int-modulo" "(" Expr "," Expr ")"            [strict]
                | "int-greater" "(" Expr "," Expr ")"           [strict]
                | "int-greater-equal" "(" Expr "," Expr ")"     [strict]
                | "int-less" "(" Expr "," Expr ")"              [strict]
                | "int-less-equal" "(" Expr "," Expr ")"        [strict]
  
  syntax Types ::= "integers"
  
  syntax Values ::= Int

  rule int-plus(I1:Int, I2:Int) => I1 +Int I2
  rule int-minus(I1:Int, I2:Int) => I1 -Int I2
  rule int-times(I1:Int, I2:Int) => I1 *Int I2
  rule int-div(I1:Int, I2:Int) => I1 /Int I2
  rule int-modulo(I1:Int, I2:Int) => I1 %Int I2
  rule int-greater(I1:Int, I2:Int) => I1 >Int I2
  rule int-greater-equal(I1:Int, I2:Int) => I1 >Int I2
  rule int-less(I1:Int, I2:Int) => I1 <Int I2
  rule int-less-equal(I1:Int, I2:Int) => I1 <=Int I2
  rule null-value(integers) => 0
  rule typed(I:Int, integers) => I
endmodule
