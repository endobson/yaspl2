(module echo2
  (import (prim bytes-length write-bytes)
          (io read-all-bytes)
          )
  (export main)
  (types)

  (define (main [stdin : InputPort] [stdout : OutputPort] [stderr : OutputPort]) : Byte
    (begin
      (let ([bytes (read-all-bytes stdin)])
        (write-bytes bytes stdout 0 (bytes-length bytes)))
      0)))
#:test-cases
(#:module-name echo2 #:stdin #"Hello world" #:stdout #"Hello world")

