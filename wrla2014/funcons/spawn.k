// Thread creation and termination

require "../funcons/expr.k"
require "../funcons/effect.k"
require "../values/functions.k"
require "../values/threads.k"

module SPAWN
  imports EXPR
  imports EFFECT
  imports FUNCTIONS
  imports THREADS

  syntax Expr ::= "spawn" "(" Expr ")"      [strict]

  rule <thread>... <k> spawn(abstraction(E:Expr)) => T ...</k> ...</thread>
       ( . => <thread>... <name> T </name> <k> effect(E) </k> ...</thread>)
    when fresh(T:Threads)

  rule (<thread>... <k> skip </k> ...</thread> => .)
endmodule
