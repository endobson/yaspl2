#lang racket/base

(require
  "machine-structs.rkt"
  "type-structs.rkt"
  "signature-structs.rkt"
  racket/match
  racket/hash
  racket/list
  (for-syntax
    racket/base
    syntax/stx
    syntax/parse
    syntax/parse/experimental/template))

(provide
  supported-primitives
  run-primitive
  primitive-module-signature)

(begin-for-syntax

  (define-syntax-class prim-ty
    #:attributes (constructor ty)
    (pattern (~datum Byte)
      #:with constructor #'byte-val
      #:with ty #'(byte-ty))
    (pattern (~datum Bytes)
      #:with constructor #'bytes-val
      #:with ty #'(bytes-ty))
    (pattern (~datum Boolean)
      #:with constructor #'boolean-val
      #:with ty #'(boolean-ty))
    (pattern (~datum InputPort)
      #:with constructor #'prim-port-val
      #:with ty #'(input-port-ty))
    (pattern (~datum OutputPort)
      #:with constructor #'prim-port-val
      #:with ty #'(output-port-ty))
    ;; These should not be used for arg types
    (pattern (~datum Void)
      #:with constructor #'(λ (_) (void-val))
      #:with ty #'(void-ty))
    (pattern (~datum Bottom)
      #:with constructor #'(λ (x) x)
      #:with ty #'(bottom-ty)))



  (define-syntax-class primitive-clause
    #:attributes (name match-clause ty)
    (pattern ((name:id (args:id (~datum :) types:prim-ty) ...) (~datum :) result-type:prim-ty body ...+)
      #:with match-clause #'[(list 'name (types.constructor args) ...) 
                             (result-type.constructor (let () body ...))]
      #:with ty #'(fun-ty empty (list types.ty ...) result-type.ty))))

(define prim-types
  (hash
    'Byte (byte-ty)
    'Bytes (bytes-ty)
    'Boolean (boolean-ty)
    'InputPort (input-port-ty)
    'OutputPort (output-port-ty)
    'Void (void-ty)
    'Bottom (bottom-ty)))


(define-syntax define-primitives
  (syntax-parser
    [(_ (supported:id run:id module-sig:id)
        clauses:primitive-clause ...)
     (template
       (begin
         (define supported (list 'clauses.name ...))
         (define module-sig 
           (module-signature 'prim
              (hash-union
                (hash (?@ 'clauses.name clauses.ty) ...)
                prim-types)
              empty))
         (define (run name args)
           (match (cons name args)
             clauses.match-clause ...))))]))

(define-primitives
  (supported-primitives run-primitive primitive-module-signature)

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

  [(write-bytes [b : Bytes] [p : OutputPort] [start-pos : Byte] [end-pos : Byte]) : Byte
   (write-bytes b p start-pos end-pos)]

  [(read-bytes [b : Bytes] [p : InputPort] [start-pos : Byte] [end-pos : Byte]) : Byte
   (define amount-read (read-bytes! b p start-pos end-pos))
   (if (eof-object? amount-read) 0 amount-read)])

