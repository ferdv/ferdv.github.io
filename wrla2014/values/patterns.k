// Pattern abstractions

require "../funcons/decl.k"
require "../values/values.k"

module PATTERNS
  imports DECL
  imports VALUES
  
  syntax Patterns ::= "abstraction" "(" Decl ")"
  
  syntax Values ::= Patterns
endmodule
