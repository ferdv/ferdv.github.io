// Streams

require "../values/types.k"
require "../values/values.k"

module STREAMS
  imports TYPES
  imports VALUES

  syntax Types ::= "streams"

  syntax Values ::= "standard-input" | "standard-output"
endmodule
