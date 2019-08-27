// Auxiliary to supply(E, K)

require "../values/values.k"

module RESET-GIVEN
  imports VALUES
  
  syntax K ::= "reset-given" "(" Values "," K ")"        [strict(2)]

  rule <k> reset-given(V:Values, V':KResult) => V' ...</k>
       <given> _ => V </given>
endmodule
