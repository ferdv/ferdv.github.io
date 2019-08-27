// Auxiliary to scoping funcons

module RESET-ENV  
  syntax K ::= "reset-env" "(" Map "," K ")"        [strict(2)]

  rule <k> reset-env(Env:Map, V':KResult) => V' ...</k>
       <env> _:Map => Env </env>
endmodule
