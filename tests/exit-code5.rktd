(module exit-code5
  (import (prim +))
  (export main)
  (types)
  (define (main [stdin : InputPort] [stdout : OutputPort] [stderr : OutputPort]) : Byte
    (+ 2 3)))
#:test-cases
(#:module-name exit-code5 #:exit-code 5)
