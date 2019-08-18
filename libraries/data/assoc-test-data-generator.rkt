#lang racket/base

(define N 16)
(for ([j 500])
  (for ([i 50])
    (define v (random))
    (if (< v .8)
        (write `(add ,(random N) ,(random N)))
        (write `(remove ,(random N))))
    (newline)
    (write `(query ,@(build-list N values)))
    (newline))

  (for ([r (in-list (sort (build-list N values) < #:key (lambda (k) (random)) #:cache-keys? #t))])
    (write `(remove ,r))
    (newline)))
