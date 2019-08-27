// Matching compound patterns to compound values

require "../funcons/decl.k"
require "../funcons/expr.k"
require "../funcons/match.k"
require "../values/patterns.k"
require "../values/tuples.k"
require "../values/values.k"

module MATCH-COMPOUND
  imports DECL
  imports EXPR
  imports MATCH
  imports PATTERNS
  imports TUPLES
  imports VALUES

  syntax Decl ::= "match-compound" "(" Expr "," Expr ")"        [strict]

  rule match-compound(tuple(.), tuple(.)) => bindings(.)
  rule match-compound(tuple(ListItem(P:Patterns) PL:List),
                      tuple(ListItem(V:Values) VL:List)) =>
         bindings-union(match(P, V),
                        match-compound(tuple(PL), tuple(VL)))
endmodule
