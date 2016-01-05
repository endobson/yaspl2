(module exit-code2
  (import)
  (export main helper)
  (types)
  (define (main stdin stdout stderr)
    (helper))
  (define (helper)
    2))
#:test-cases
(#:module-name exit-code2 #:exit-code 2)
