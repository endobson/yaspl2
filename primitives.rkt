#lang racket/base

(require
  "machine-structs.rkt"
  racket/match
  (for-syntax
    racket/base
    syntax/stx
    syntax/parse))

(provide
  supported-primitives
  run-primitive)

(begin-for-syntax
  (define-syntax-class primitive-clause
    #:attributes (name match-clause)
    (pattern ((name:id args:expr ...) body ...+)
      #:with match-clause #'[(list 'name args ...) body ...])))


(define-syntax define-primitives
  (syntax-parser
    [(_ (supported:id run:id)
        clauses:primitive-clause ...)
     #`(begin
         (define supported (list 'clauses.name ...))
         (define (run name args)
           (match (cons name args)
             clauses.match-clause ...)))]))



(define-primitives
  (supported-primitives run-primitive)

  [(or (boolean-val x) (boolean-val y))
   (boolean-val (or x y))]

  [(+ (byte-val x) (byte-val y))
   (byte-val (+ x y))]
  [(- (byte-val x) (byte-val y))
   (byte-val (- x y))]
  [(* (byte-val x) (byte-val y))
   (byte-val (* x y))]
  [(= (byte-val x) (byte-val y))
   (boolean-val (= x y))]

  [(void)
   (void-val)]

  [(panic (bytes-val bytes))
   (error-sentinal bytes)]

  [(make-bytes (byte-val size))
   (bytes-val (make-bytes size))]
  [(bytes-ref (bytes-val b) (byte-val index))
   (byte-val (bytes-ref b index))]
  [(bytes-set! (bytes-val b) (byte-val index) (byte-val v))
   (bytes-set! b index v)
   (void-val)]
  [(bytes-length (bytes-val b))
   (byte-val (bytes-length b))]

  [(write-byte (byte-val x) (prim-port-val p))
   (write-byte x p)
   (void-val)]
  [(read-bytes (bytes-val b) (prim-port-val p) (byte-val start-pos) (byte-val end-pos))
   (define amount-read (read-bytes! b p start-pos end-pos))
   (byte-val (if (eof-object? amount-read) 0 amount-read))])

