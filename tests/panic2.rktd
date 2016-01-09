(module panic2
  (import (prim panic make-bytes))
  (export main)
  (types)
  (define (main [stdin : InputPort] [stdout : OutputPort] [stderr : OutputPort]) : Byte
    (panic #"Boom")))
#:test-cases
(#:module-name panic2 #:exit-code 255 #:error #"Boom")
