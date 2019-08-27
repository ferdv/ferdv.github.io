// Unit type

require "../values/types.k"
require "../values/values.k"

module UNIT
  imports TYPES
  imports VALUES

  syntax Unit ::= "null"
  
  syntax Types ::= "unit"
  
  syntax Values ::= Unit
  
  rule typed(null, unit) => null
endmodule
