// Pure closed expressions

require "cinf-syntax.k"
require "cinf-expressions-funcons.k"
require "cinf-expressions-values.k"

module CINF

  imports CINF-SYNTAX
  imports CINF-EXPRESSIONS-FUNCONS
  imports CINF-EXPRESSIONS-VALUES

  syntax Expr ::= "value" "[[" Literal "]]"                           [function]

  rule value[[ I:Int ]] => I
  rule value[[ true ]] => true
  rule value[[ false ]] => false


  syntax Expr ::= "evaluate" "[[" Expression "]]"                     [function]

  rule evaluate[[ L:Literal ]] => value[[ L ]]

  rule evaluate[[ + E:Expression ]] => int-plus(0, evaluate[[ E ]])
  rule evaluate[[ - E:Expression ]] => int-minus(0, evaluate[[ E ]])
  rule evaluate[[ ! E:Expression ]] => not(evaluate[[ E ]])

  rule evaluate[[ E1:Expression * E2:Expression ]] =>
    int-times(evaluate[[ E1 ]], evaluate[[ E2 ]])
  rule evaluate[[ E1:Expression / E2:Expression ]] =>
    int-div(evaluate[[ E1 ]], evaluate[[ E2 ]])
  rule evaluate[[ E1:Expression % E2:Expression ]] =>
    int-modulo(evaluate[[ E1 ]], evaluate[[ E2 ]])
  rule evaluate[[ E1:Expression + E2:Expression ]] =>
    int-plus(evaluate[[ E1 ]], evaluate[[ E2 ]])
  rule evaluate[[ E1:Expression - E2:Expression ]] =>
    int-minus(evaluate[[ E1 ]], evaluate[[ E2 ]])
  rule evaluate[[ E1:Expression < E2:Expression ]] =>
    int-less(evaluate[[ E1 ]], evaluate[[ E2 ]])
  rule evaluate[[ E1:Expression > E2:Expression ]] =>
    int-greater(evaluate[[ E1 ]], evaluate[[ E2 ]])
  rule evaluate[[ E1:Expression <= E2:Expression ]] =>
    int-less-equal(evaluate[[ E1 ]], evaluate[[ E2 ]])
  rule evaluate[[ E1:Expression >= E2:Expression ]] =>
    int-greater-equal(evaluate[[ E1 ]], evaluate[[ E2 ]])
  rule evaluate[[ E1:Expression != E2:Expression ]] =>
    not(equal(evaluate[[ E1 ]], evaluate[[ E2 ]]))
  rule evaluate[[ E1:Expression == E2:Expression ]] =>
    equal(evaluate[[ E1 ]], evaluate[[ E2 ]])
  rule evaluate[[ E1:Expression && E2:Expression ]] =>
    if-true(evaluate[[ E1 ]], evaluate[[ E2 ]], false)
  rule evaluate[[ E1:Expression || E2:Expression ]] =>
    if-true(evaluate[[ E1 ]], true, evaluate[[ E2 ]])

  configuration
    <T>
      <k> evaluate[[$PGM:Expression]] </k>
    </T>

endmodule
