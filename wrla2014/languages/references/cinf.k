// Reference declarations and parameters, extending threads

require "cinf-syntax.k"
require "cinf-references-funcons.k"
require "cinf-references-values.k"

module CINF

  imports CINF-SYNTAX
  imports CINF-REFERENCES-FUNCONS
  imports CINF-REFERENCES-VALUES

  syntax Expr ::= "value" "[[" Literal "]]"                           [function]

  rule value[[ I:Int ]] => I
  rule value[[ S:String ]] => S
  rule value[[ true ]] => true
  rule value[[ false ]] => false
  rule value[[ cout ]] => standard-output
  rule value[[ cin ]] => standard-input
  rule value[[ endl ]] => "\n"

  syntax Id ::= "returned"

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

  rule evaluate_rval[[ E1:Expression ( E2:Expression ) ]] =>
    apply(evaluate_rval[[ E1 ]], evaluate_params[[ tuple(E2) ]])
  rule evaluate_rval[[ E1:Expression (  ) ]] =>
    apply(evaluate_rval[[ E1 ]], tuple(.))

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
  rule evaluate_rval[[ E1:Expression << E2:Expression ]] =>
    output(evaluate_rval[[ E1 ]], evaluate_rval[[ E2 ]])
  rule evaluate_rval[[ E1:Expression >> E2:Expression ]] =>
    input(evaluate_rval[[ E1 ]], evaluate_lval[[ E2 ]])

  rule evaluate_rval[[ E1:Expression , E2:Expression ]] =>
    seq(effect(evaluate[[ E1 ]]), evaluate_rval[[ E2 ]])


  syntax Expr ::= "evaluate_params" "[[" ExpressionList "]]"          [function]

  syntax ExpressionList ::= "tuple" "(" Expression ")"

  rule evaluate_params[[ tuple(E1:Expression ,  E2:Expression) ]] =>
    tuple-prefix(evaluate[[ E1 ]], evaluate_params[[ tuple(E2) ]])
  rule evaluate_params[[ tuple(E:Expression) ]] =>
    tuple-prefix(evaluate[[ E ]], tuple(.)) [owise]


  syntax Comm ::= "execute" "[[" StatementSeq "]]"                    [function]

  rule execute[[ E:Expression ; ]] => effect(evaluate[[ E ]])
  rule execute[[ ; ]] => skip

  rule execute[[ { SS:StatementSeq } ]] => execute[[ SS ]]
  rule execute[[ { } ]] => skip

  rule execute[[ if ( E:Expression ) S:Statement ]] =>
    execute[[ if ( E ) S else { } ]]
  rule execute[[ if ( E:Expression ) S1:Statement else S2:Statement ]] =>
    if-true(evaluate_rval[[ E ]], execute[[ S1 ]], execute[[ S2 ]])

  rule execute[[ while ( E:Expression ) S:Statement ]] =>
    while-true(evaluate_rval[[ E ]], execute[[ S ]])

  rule execute[[ return E:Expression; ]] =>
    throw(variant(returned, evaluate[[ E ]]))
  rule execute[[ return ; ]] =>
    throw(variant(returned, null))

  rule execute[[ BD:BlockDeclaration ]] => effect(elaborate[[ BD ]])

  rule execute[[ BS:BlockStatement SS:StatementSeq ]] =>
    seq(execute[[ BS ]], execute[[ SS ]])

  rule execute[[ BD:BlockDeclaration SS:StatementSeq ]] =>
    scope(elaborate[[ BD ]], execute[[ SS ]])


  syntax Decl ::= "elaborate_forwards" "[[" DeclarationSeq "]]"       [function]

  rule elaborate_forwards[[ # include <iostream> ]] => bindings(.)
  rule elaborate_forwards[[ _:BlockDeclaration ]] => bindings(.)

  rule elaborate_forwards[[ T:TypeSpecifier I:Id
                            ( PDL:ParameterDeclarationList )
                            CS:CompoundStatement ]] =>
    bind-value(I,
      allocate(variables(functions(type_tuple[[ PDL ]], type[[ T ]]))))

  rule elaborate_forwards[[ D:Declaration DS:DeclarationSeq ]] =>
    accum(elaborate_forwards[[ D ]], elaborate_forwards[[ DS ]])


  syntax Decl ::= "elaborate" "[[" DeclarationSeq "]]"                [function]

  rule elaborate[[ # include <iostream> ]] => bindings(.)
  rule elaborate[[ using namespace std ; ]] => bindings(.)

  rule elaborate[[ T:TypeSpecifier I:Id ; ]] =>
    accum(bind-value(I, allocate(variables(type[[ T ]]))),
      decl-effect(assign(bound-value(I), 0)))

  rule elaborate[[ T:TypeSpecifier I:Id = E:Expression ; ]] =>
    accum(elaborate[[ T I ;]],
      decl-effect(assign(bound-value(I), evaluate_rval[[ E ]])))

  rule elaborate[[ T:TypeSpecifier & I:Id = E:Expression ; ]] =>
    bind-value(I, evaluate_lval[[ E ]])

  rule elaborate[[ T:TypeSpecifier ID:InitDeclarator ,
                                   IDL:InitDeclaratorList ; ]] =>
    accum(elaborate[[ T ID ; ]], elaborate[[ T IDL ; ]])

  rule elaborate[[ std::thread I1:Id ( I2:Id ) ; ]] =>
    decl-effect(effect(
      spawn(close(abstraction(evaluate[[ I2 () ]])))))

  rule elaborate[[ std::thread I1:Id ( I2:Id , E:Expression ) ; ]] =>
    decl-effect(effect(
      spawn(close(abstraction(evaluate[[ I2 (E) ]])))))

  rule elaborate[[ T:TypeSpecifier I:Id ( PDL:ParameterDeclarationList )
                   CS:CompoundStatement ]] =>
    decl-effect(assign(bound-value(I),
        close(abstraction(
          scope(match-compound(pattern_tuple[[ PDL ]], given),
            catch(seq(execute[[ CS ]], throw(variant(returned, null))),
              abstraction(current-value(original(returned, given)))))))))

  rule elaborate[[ T:TypeSpecifier & I:Id ( PDL:ParameterDeclarationList )
                   CS:CompoundStatement ]] =>
    decl-effect(assign(bound-value(I),
        close(abstraction(
          scope(match-compound(pattern_tuple[[ PDL ]], given),
            catch(seq(execute[[ CS ]], throw(variant(returned, null))),
              abstraction(original(returned, given))))))))

  rule elaborate[[ D:Declaration DS:DeclarationSeq ]] =>
    accum(elaborate[[ D ]], elaborate[[ DS ]])


  syntax Tuples ::= "type_tuple" "[[" ParameterDeclarationList "]]"   [function]

  rule type_tuple[[ .ParameterDeclarationList ]] => tuple(.)
  rule type_tuple[[ T:TypeSpecifier D:Declarator ,
                    PDL:ParameterDeclarationList ]] =>
    tuple-prefix(type[[ T ]], type_tuple[[ PDL ]])
  rule type_tuple[[ T:TypeSpecifier AD:AbstractDeclarator ,
                    PDL:ParameterDeclarationList ]] =>
    tuple-prefix(type[[ T ]], type_tuple[[ PDL ]])


  syntax Types ::= "type" "[[" TypeSpecifier "]]"                     [function]

  rule type[[ bool ]] => booleans
  rule type[[ int ]] => integers
  rule type[[ void ]] => unit


  syntax Tuples ::= "pattern_tuple" "[[" ParameterDeclarationList "]]"[function]

  rule pattern_tuple[[ .ParameterDeclarationList ]] => tuple(.)
  rule pattern_tuple[[ PD:ParameterDeclaration ,
                       PDL:ParameterDeclarationList ]] =>
    tuple-prefix(pattern[[ PD ]], pattern_tuple[[ PDL ]])


  syntax Patterns ::= "pattern" "[[" ParameterDeclaration "]]"        [function]

  rule pattern[[ T:TypeSpecifier I:Id ]] =>
    abstraction(
      accum(bind-value(I, allocate(variables(type[[ T ]]))),
        decl-effect(assign(bound-value(I), current-value(given)))))

  rule pattern[[ T:TypeSpecifier & I:Id ]] =>
    abstraction(bind-value(I, given))


  syntax Comm ::= "translate" "[[" TranslationUnit "]]"               [function]

  rule translate[[ DS:DeclarationSeq ]] =>
    scope(accum(elaborate_forwards[[ DS ]], elaborate[[ DS ]]),
      effect(apply(evaluate_rval[[ main ]], tuple(.))))

  configuration
    <T>
      <threads>
        <thread multiplicity="*">
          <name> main:Threads </name>
          <k> translate[[$PGM:TranslationUnit]] </k>
          <xstack> .List </xstack>
          <context>
            <env> .Map </env>
            <given> no-value </given>
          </context>
        </thread>
      </threads>
      <store> .Map </store>
      <output stream="stdout"> .List </output>
      <input stream="stdin"> .List </input>
    </T>

endmodule
