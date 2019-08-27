// Functions, extending statements

module CINF-SYNTAX

  syntax Literal ::= Int | String | Bool | "cout" | "cin" | "endl"

  syntax Id ::= "main"

  syntax Expression ::=
      Literal
    | Id
    | "(" Expression ")" [bracket]
    | Expression "(" Expression ")"
    | Expression "(" ")"
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
    | CompoundStatement
    | "if" "(" Expression ")" Statement
    | "if" "(" Expression ")" Statement "else" Statement
    | "while" "(" Expression ")" Statement
    | "return" Expression ";"
    | "return" ";"

  syntax CompoundStatement ::=
      "{" StatementSeq "}"
    | "{" "}"

  syntax DeclarationSeq ::=
      Declaration
    | Declaration DeclarationSeq

  syntax Declaration ::=
      "#" "include" "<iostream>"
    | BlockDeclaration
    | TypeSpecifier Id "(" ParameterDeclarationList ")" CompoundStatement

  syntax BlockDeclaration ::=
      "using" "namespace" "std" ";"
    | TypeSpecifier InitDeclaratorList ";"

  syntax TypeSpecifier ::=
      "bool"
    | "int"
    | "void"

  syntax InitDeclaratorList ::=
      InitDeclarator
    | InitDeclarator "," InitDeclaratorList

  syntax InitDeclarator ::=
      Id
    | Id "=" Expression

  syntax ParameterDeclarationList ::=
    List{ParameterDeclaration, ","}

  syntax ParameterDeclaration ::=
    TypeSpecifier Id

  syntax TranslationUnit ::=
      DeclarationSeq

endmodule
