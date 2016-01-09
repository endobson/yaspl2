(module stdin1
  (import (prim make-bytes read-bytes bytes-ref))
  (export main)
  (types)

  (define (main [stdin : InputPort] [stdout : OutputPort] [stderr : OutputPort]) : Byte
    (let ([bytes (make-bytes 5)])
      (begin
        (read-bytes bytes stdin 0 5)
        (bytes-ref bytes 0)))))
#:test-cases
(#:module-name stdin1 #:stdin #"A" #:exit-code 65)
