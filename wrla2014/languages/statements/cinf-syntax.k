// Statements, extending declarations

module CINF-SYNTAX

  syntax Literal ::= Int | String | Bool | "cout" | "cin" | "endl"

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
    > left:
      Expression "<<" Expression [left]
    | Expression ">>" Expression [left]
    > Expression "=" Expression [right]
    > Expression "," Expression [right]

  syntax UnaryOperator ::=
      "+" | "-" | "!"

  syntax "+"  -/- [\+]
  syntax "++" -/- [\+]
  syntax "-"  -/- [\-]
  syntax "--" -/- [\-]

  syntax StatementSeq ::=
      Statement
    | Statement StatementSeq

  syntax Statement ::=
      BlockStatement
    | BlockDeclaration

  syntax BlockStatement ::=
      Expression ";"
    | ";"
    | "{" StatementSeq "}"
    | "{" "}"
    | "if" "(" Expression ")" Statement
    | "if" "(" Expression ")" Statement "else" Statement
    | "while" "(" Expression ")" Statement

  syntax DeclarationSeq ::=
      Declaration
    | Declaration DeclarationSeq

  syntax Declaration ::=
      "#" "include" "<iostream>"
    | BlockDeclaration

  syntax BlockDeclaration ::=
      "using" "namespace" "std" ";"
    | TypeSpecifier InitDeclaratorList ";"

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
