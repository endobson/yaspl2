(module exit-code4
  (import)
  (export main)
  (types)
  (define (main [stdin : InputPort] [stdout : OutputPort] [stderr : OutputPort]) : Byte
    (if #t 4 5)))
#:test-cases
(#:module-name exit-code4 #:exit-code 4)
