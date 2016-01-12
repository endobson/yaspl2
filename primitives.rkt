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

  (define-syntax-class prim-ty
    #:attributes (constructor)
    (pattern (~datum Byte)
      #:with constructor #'byte-val)
    (pattern (~datum Bytes)
      #:with constructor #'bytes-val)
    (pattern (~datum Boolean)
      #:with constructor #'boolean-val)
    (pattern (~datum InputPort)
      #:with constructor #'prim-port-val)
    (pattern (~datum OutputPort)
      #:with constructor #'prim-port-val)
    ;; These should not be used for arg types
    (pattern (~datum Void)
      #:with constructor #'(λ (_) (void-val)))
    (pattern (~datum Bottom)
      #:with constructor #'(λ (x) x)))



  (define-syntax-class primitive-clause
    #:attributes (name match-clause)
    (pattern ((name:id (args:id (~datum :) types:prim-ty) ...) (~datum :) result-type:prim-ty body ...+)
      #:with match-clause #'[(list 'name (types.constructor args) ...) 
                             (result-type.constructor (let () body ...))])))


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

  [(or [x : Boolean] (y : Boolean)) : Boolean (or x y)]
  [(and [x : Boolean] (y : Boolean)) : Boolean (and x y)]

  [(+ [x : Byte] (y : Byte)) : Byte (+ x y)]
  [(- [x : Byte] (y : Byte)) : Byte (- x y)]
  [(* [x : Byte] (y : Byte)) : Byte (* x y)]
  [(quotient [x : Byte] (y : Byte)) : Byte (quotient x y)]
  [(remainder [x : Byte] (y : Byte)) : Byte (remainder x y)]

  [(= [x : Byte] (y : Byte)) : Boolean (= x y)]
  [(<= [x : Byte] (y : Byte)) : Boolean (<= x y)]
  [(>= [x : Byte] (y : Byte)) : Boolean (>= x y)]
  [(< [x : Byte] (y : Byte)) : Boolean (< x y)]
  [(> [x : Byte] (y : Byte)) : Boolean (> x y)]

  [(void) : Void (void)]

  [(panic [bytes : Bytes]) : Bottom (error-sentinal bytes)]

  [(make-bytes [size : Byte]) : Bytes (make-bytes size)]
  [(bytes-ref [b : Bytes] [index : Byte]) : Byte (bytes-ref b index)]
  [(bytes-set! [b : Bytes] [index : Byte] [v : Byte]) : Void (bytes-set! b index v)]
  [(bytes-length [b : Bytes]) : Byte (bytes-length b)]

  [(write-bytes [b : Bytes] [p : InputPort] [start-pos : Byte] [end-pos : Byte]) : Byte
   (write-bytes b p start-pos end-pos)]

  [(read-bytes [b : Bytes] [p : InputPort] [start-pos : Byte] [end-pos : Byte]) : Byte
   (define amount-read (read-bytes! b p start-pos end-pos))
   (if (eof-object? amount-read) 0 amount-read)])

