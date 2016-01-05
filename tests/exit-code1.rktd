(module exit-code
  (import)
  (export main)
  (types)
  (define (main stdin stdout stderr)
    1))
#:test-cases
(#:module-name exit-code #:exit-code 1)
