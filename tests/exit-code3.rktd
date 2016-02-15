(module exit-code3
  (import)
  (export main helper)
  (types)
  (define (main [args : (Array Bytes)] [stdin : InputPort] [stdout : OutputPort] [stderr : OutputPort]) : Byte
    (helper 3))
  (define (helper [x : Byte]) : Byte
    x))
#:test-cases
(#:module-name exit-code3 #:exit-code 3)
