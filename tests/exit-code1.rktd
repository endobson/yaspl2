(module exit-code
  (import)
  (export main)
  (types)
  (define (main [args : (Array Bytes)] [stdin : InputPort] [stdout : OutputPort] [stderr : OutputPort]) : Byte
    1))
#:test-cases
(#:module-name exit-code #:exit-code 1)
