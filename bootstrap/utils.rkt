#lang racket/base

(provide
  hash-copy/immutable)


(define (hash-copy/immutable env)
  (make-immutable-hash (hash->list env)))
