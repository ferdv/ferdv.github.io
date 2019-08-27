// Pointer declarations and expressions, extending references

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
    | "new" TypeSpecifier
    | "new" TypeSpecifier "*"
    | "delete" Expression
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
      "*" | "&" | "+" | "-" | "!"

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
    | FunType Declarator CompoundStatement

  syntax BlockDeclaration ::=
      "using" "namespace" "std" ";"
    | FunType InitDeclaratorList ";"
    | "std::thread" Id "(" Id "," Expression ")" ";"
    | "std::thread" Id "(" Id ")" ";"

  syntax FunType ::= // Auxiliary syntax, not in C++11
      TypeSpecifier
    | "(" ParameterDeclarationList ")" "-->" FunType
    | "(" FunType "*" ")"
    | "(" FunType "&" ")"
    | "(" FunType ")"   [bracket]

  syntax TypeSpecifier ::=
      "bool"
    | "int"
    | "void"

  syntax InitDeclaratorList ::=
      InitDeclarator
    | InitDeclarator "," InitDeclaratorList

  syntax InitDeclarator ::=
      Declarator
    | Declarator "=" Expression

  syntax Declarator ::=
      Id
    | "&" Id
    | "(" Declarator ")" [bracket]
    | Declarator "(" ParameterDeclarationList ")"
    > "*" Declarator

  syntax ParameterDeclarationList ::=
    List{ParameterDeclaration, ","}

  syntax ParameterDeclaration ::=
      FunType Declarator
    | FunType AbstractDeclarator

  syntax AbstractDeclarator ::=
      "*"
    | "&"
    | "(" AbstractDeclarator ")" [bracket]
    | AbstractDeclarator "(" ParameterDeclarationList ")"
    > "*" AbstractDeclarator

  syntax TranslationUnit ::=
      DeclarationSeq

endmodule
