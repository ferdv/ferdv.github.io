// Variable declarations, extending expressions

require "cinf-syntax.k"
require "cinf-declarations-funcons.k"
require "cinf-declarations-values.k"

module CINF

  imports CINF-SYNTAX
  imports CINF-DECLARATIONS-FUNCONS
  imports CINF-DECLARATIONS-VALUES

  syntax Expr ::= "value" "[[" Literal "]]"                           [function]

  rule value[[ I:Int ]] => I
  rule value[[ true ]] => true
  rule value[[ false ]] => false


  // default evaluation
  //   - evaluate lvalue expressions to an lvalue
  //   - evaluate other expressions to an rvalue
  syntax Expr ::= "evaluate" "[[" Expression "]]"                     [function]

  rule evaluate[[ I:Id ]] => evaluate_lval[[ I:Id ]]

  rule evaluate[[ ++ E:Expression ]] => evaluate_lval[[ ++ E:Expression ]]

  rule evaluate[[ -- E:Expression ]] => evaluate_lval[[ -- E:Expression ]]

  rule evaluate[[ E1:Expression = E2:Expression ]] =>
    evaluate_lval[[ E1:Expression = E2:Expression ]]

  rule evaluate[[ E1:Expression , E2:Expression ]] =>
    seq(effect(evaluate[[ E1 ]]), evaluate[[ E2 ]])

  rule evaluate[[ E:Expression ]] => evaluate_rval[[ E ]]


  // lvalue evaluation (undefined if parameter is not an lvalue-expression)
  syntax Expr ::= "evaluate_lval" "[[" Expression "]]"                [function]

  rule evaluate_lval[[ I:Id ]] => bound-value(I)

  rule evaluate_lval[[ ++ E:Expression ]] =>
    supply(evaluate_lval[[ E ]],
      assign-giving-variable(given, int-plus(stored-value(given), 1)))

  rule evaluate_lval[[ -- E:Expression ]] =>
    supply(evaluate_lval[[ E ]],
      assign-giving-variable(given, int-minus(stored-value(given), 1)))

  rule evaluate_lval[[ E1:Expression = E2:Expression ]] =>
    assign-giving-variable(evaluate_lval[[ E1 ]], evaluate_rval[[ E2 ]])

  rule evaluate_lval[[ E1:Expression , E2:Expression ]] =>
    seq(effect(evaluate[[ E1 ]]), evaluate_lval[[ E2 ]])


  // rvalue evaluation
  syntax Expr ::= "evaluate_rval" "[[" Expression "]]"                [function]

  rule evaluate_rval[[ L:Literal ]] => value[[ L ]]
  rule evaluate_rval[[ I:Id ]] =>
    stored-value(evaluate_lval[[ I ]])
  rule evaluate_rval[[ E1:Expression = E2:Expression ]] =>
    stored-value(evaluate_lval[[ E1 = E2 ]])

  rule evaluate_rval[[ ++ E:Expression ]] =>
    stored-value(evaluate_lval[[ ++ E ]])

  rule evaluate_rval[[ -- E:Expression ]] =>
    stored-value(evaluate_lval[[ -- E ]])

  rule evaluate_rval[[ E:Expression ++ ]] =>
    supply(evaluate_lval[[ E ]],
      assign-giving-current-value(given, int-plus(stored-value(given), 1)))

  rule evaluate_rval[[ E:Expression -- ]] =>
    supply(evaluate_lval[[ E ]],
      assign-giving-current-value(given, int-minus(stored-value(given), 1)))

  rule evaluate_rval[[ + E:Expression ]] => int-plus(0, evaluate_rval[[ E ]])
  rule evaluate_rval[[ - E:Expression ]] => int-minus(0, evaluate_rval[[ E ]])
  rule evaluate_rval[[ ! E:Expression ]] => not(evaluate_rval[[ E ]])

  rule evaluate_rval[[ E1:Expression * E2:Expression ]] =>
    int-times(evaluate_rval[[ E1 ]], evaluate_rval[[ E2 ]])
  rule evaluate_rval[[ E1:Expression / E2:Expression ]] =>
    int-div(evaluate_rval[[ E1 ]], evaluate_rval[[ E2 ]])
  rule evaluate_rval[[ E1:Expression % E2:Expression ]] =>
    int-modulo(evaluate_rval[[ E1 ]], evaluate_rval[[ E2 ]])
  rule evaluate_rval[[ E1:Expression + E2:Expression ]] =>
    int-plus(evaluate_rval[[ E1 ]], evaluate_rval[[ E2 ]])
  rule evaluate_rval[[ E1:Expression - E2:Expression ]] =>
    int-minus(evaluate_rval[[ E1 ]], evaluate_rval[[ E2 ]])
  rule evaluate_rval[[ E1:Expression < E2:Expression ]] =>
    int-less(evaluate_rval[[ E1 ]], evaluate_rval[[ E2 ]])
  rule evaluate_rval[[ E1:Expression > E2:Expression ]] =>
    int-greater(evaluate_rval[[ E1 ]], evaluate_rval[[ E2 ]])
  rule evaluate_rval[[ E1:Expression <= E2:Expression ]] =>
    int-less-equal(evaluate_rval[[ E1 ]], evaluate_rval[[ E2 ]])
  rule evaluate_rval[[ E1:Expression >= E2:Expression ]] =>
    int-greater-equal(evaluate_rval[[ E1 ]], evaluate_rval[[ E2 ]])
  rule evaluate_rval[[ E1:Expression != E2:Expression ]] =>
    not(equal(evaluate_rval[[ E1 ]], evaluate_rval[[ E2 ]]))
  rule evaluate_rval[[ E1:Expression == E2:Expression ]] =>
    equal(evaluate_rval[[ E1 ]], evaluate_rval[[ E2 ]])
  rule evaluate_rval[[ E1:Expression && E2:Expression ]] =>
    if-true(evaluate_rval[[ E1 ]], evaluate_rval[[ E2 ]], false)
  rule evaluate_rval[[ E1:Expression || E2:Expression ]] =>
    if-true(evaluate_rval[[ E1 ]], true, evaluate_rval[[ E2 ]])

  rule evaluate_rval[[ E1:Expression , E2:Expression ]] =>
    seq(effect(evaluate[[ E1 ]]), evaluate_rval[[ E2 ]])


  syntax Decl ::= "elaborate" "[[" DeclarationSeq "]]"                [function]

  rule elaborate[[ T:TypeSpecifier I:Id ; ]] =>
    accum(bind-value(I, allocate(variables(type[[ T ]]))),
      decl-effect(assign(bound-value(I), 0)))

  rule elaborate[[ T:TypeSpecifier I:Id = E:Expression ; ]] =>
    accum(elaborate[[ T I ;]],
      decl-effect(assign(bound-value(I), evaluate_rval[[ E ]])))

  rule elaborate[[ T:TypeSpecifier ID:InitDeclarator ,
                                   IDL:InitDeclaratorList ; ]] =>
    accum(elaborate[[ T ID ; ]], elaborate[[ T IDL ; ]])

  rule elaborate[[ D:Declaration DS:DeclarationSeq ]] =>
    accum(elaborate[[ D ]], elaborate[[ DS ]])


  syntax Types ::= "type" "[[" TypeSpecifier "]]"                     [function]

  rule type[[ bool ]] => booleans
  rule type[[ int ]] => integers


  configuration
    <T>
      <k> elaborate[[$PGM:DeclarationSeq]] </k>
      <env> .Map </env>
      <given> no-value </given>
      <store> .Map </store>
    </T>

endmodule
