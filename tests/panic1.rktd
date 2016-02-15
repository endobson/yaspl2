(module panic1
  (import (prim panic make-bytes))
  (export main)
  (types)
  (define (main [args : (Array Bytes)] [stdin : InputPort] [stdout : OutputPort] [stderr : OutputPort]) : Byte
    (panic (make-bytes 3))))
#:test-cases
(#:module-name panic1 #:exit-code 255 #:error #"\0\0\0")
