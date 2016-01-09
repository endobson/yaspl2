(module exit-code2
  (import)
  (export main helper)
  (types)
  (define (main [stdin : InputPort] [stdout : OutputPort] [stderr : OutputPort]) : Byte
    (helper))
  (define (helper) : Byte
    2))
#:test-cases
(#:module-name exit-code2 #:exit-code 2)
