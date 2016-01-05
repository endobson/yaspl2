(module exit-code3
  (import)
  (export main helper)
  (types)
  (define (main stdin stdout stderr)
    (helper 3))
  (define (helper x)
    x))
#:test-cases
(#:module-name exit-code3 #:exit-code 3)
