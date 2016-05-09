(module exit-code6
  (import (exit-code6-helper times2))
  (export
    #:types ()
    #:values (main)
    #:patterns ())
  (types)
  (define (main [args : (Array Bytes)] [stdin : InputPort] [stdout : OutputPort] [stderr : OutputPort]) : Byte
    (times2 3)))
(module exit-code6-helper
  (import (prim +))
  (export
    #:types ()
    #:values (times2)
    #:patterns ())
  (types)
  (define (times2 [x : Byte]) : Byte
    (+ x x)))
#:test-cases
(#:module-name exit-code6 #:exit-code 6)
