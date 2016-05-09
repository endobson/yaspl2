(module panic2
  (import (prim panic make-bytes))
  (export
    #:types ()
    #:values (main)
    #:patterns ())
  (types)
  (define (main [args : (Array Bytes)] [stdin : InputPort] [stdout : OutputPort] [stderr : OutputPort]) : Byte
    (panic #"Boom")))
#:test-cases
(#:module-name panic2 #:exit-code 255 #:error #"Boom")
