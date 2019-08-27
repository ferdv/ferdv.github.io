// Variable declarations, extending expressions

module CINF-SYNTAX

  syntax Literal ::= Int | Bool

  syntax Expression ::=
      Literal
    | Id
    | "(" Expression ")" [bracket]
    | "++" Expression
    | "--" Expression
    | Expression "++"
    | Expression "--"
    > UnaryOperator Expression
    > left:
      Expression "*" Expression [left]
    | Expression "/" Expression [left]
    | Expression "%" Expression [left]
    > left:
      Expression "+" Expression [left]
    | Expression "-" Expression [left]
    > left:
      Expression "<" Expression [left]
    | Expression ">" Expression [left]
    | Expression "<=" Expression [left]
    | Expression ">=" Expression [left]
    | Expression "!=" Expression [left]
    | Expression "==" Expression [left]
    > Expression "&&" Expression [left]
    > Expression "||" Expression [left]
    > Expression "=" Expression [right]
    > Expression "," Expression [right]

  syntax UnaryOperator ::=
      "+" | "-" | "!"

  syntax "+"  -/- [\+]
  syntax "++" -/- [\+]
  syntax "-"  -/- [\-]
  syntax "--" -/- [\-]

  syntax DeclarationSeq ::=
      Declaration
    | Declaration DeclarationSeq

  syntax Declaration ::=
      BlockDeclaration

  syntax BlockDeclaration ::=
      TypeSpecifier InitDeclaratorList ";"

  syntax TypeSpecifier ::=
      "bool"
    | "int"

  syntax InitDeclaratorList ::=
      InitDeclarator
    | InitDeclarator "," InitDeclaratorList

  syntax InitDeclarator ::=
      Id
    | Id "=" Expression

endmodule
