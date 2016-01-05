(module panic2
  (import (prim panic make-bytes))
  (export main)
  (types)
  (define (main stdin stdout stderr)
    (panic #"Boom")))
#:test-cases
(#:module-name panic2 #:exit-code 255 #:error #"Boom")
