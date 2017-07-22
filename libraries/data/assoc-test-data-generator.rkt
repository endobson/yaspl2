#lang racket/base

(define N 128)
(for ([j 10])
  (for ([i 1000])
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
