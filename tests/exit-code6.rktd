(module exit-code6
  (import (exit-code6-helper times2))
  (export main)
  (types)
  (define (main [args : (Array Bytes)] [stdin : InputPort] [stdout : OutputPort] [stderr : OutputPort]) : Byte
    (times2 3)))
(module exit-code6-helper
  (import (prim +))
  (export times2)
  (types)
  (define (times2 [x : Byte]) : Byte
    (+ x x)))
#:test-cases
(#:module-name exit-code6 #:exit-code 6)
