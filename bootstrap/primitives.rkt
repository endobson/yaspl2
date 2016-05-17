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
  primitive-module-signature)

(begin-for-syntax

  (define-syntax-class prim-ty
    #:attributes (ty)
    (pattern (~datum Byte)
      #:with ty #'(byte-ty))
    (pattern (~datum Bytes)
      #:with ty #'(bytes-ty))
    (pattern (~datum Boolean)
      #:with ty #'(boolean-ty))
    (pattern (~datum InputPort)
      #:with ty #'(input-port-ty))
    (pattern (~datum OutputPort)
      #:with ty #'(output-port-ty))

    (pattern ((~datum Box) v:id)
      #:with ty #'(box-ty (type-var-ty 'v)))
    (pattern ((~datum Array) v:id)
      #:with ty #'(array-ty (type-var-ty 'v)))
    (pattern ((~datum type-var) v:id)
      #:with ty #'(type-var-ty 'v))

    ;; These should not be used for arg types
    (pattern (~datum Void)
      #:with ty #'(void-ty)))



  (define-syntax-class primitive-clause
    #:attributes (name impl ty)
    (pattern ((name:id (args:id (~datum :) types:prim-ty) ...)
              (~datum :) result-type:prim-ty body:expr ...+)
      #:with impl #'(lambda (args ...) body ...)

      #:with ty #'(fun-ty empty (list types.ty ...) result-type.ty))
    (pattern ((type-vars:id ...)
              (name:id (args:id (~datum :) types:prim-ty) ...)
              (~datum :) result-type:prim-ty body:expr ...+)
      #:with impl #'(lambda (args ...) body ...)
      #:with ty #'(fun-ty (list 'type-vars ...) (list types.ty ...) result-type.ty))
    (pattern ((type-vars:id ...)
              (name:id (args:id (~datum :) types:prim-ty) ...)
              (~datum :) result-type:prim-ty #:error message:expr)
      #:with impl
        #'(lambda (args ...) ((exit-parameter) (error-sentinal message)))
      #:with ty #'(fun-ty (list 'type-vars ...) (list types.ty ...) result-type.ty))))

(define prim-types
  (hash
    'Byte (byte-ty)
    'Bytes (bytes-ty)
    'Boolean (boolean-ty)
    'InputPort (input-port-ty)
    'OutputPort (output-port-ty)
    'Void (void-ty)
    'Box (box-ty-constructor)
    'Array (array-ty-constructor)))


(define-syntax define-primitives
  (syntax-parser
    [(_ (supported:id module-sig:id)
        clauses:primitive-clause ...)
     (template
       (begin
         (define supported
           (hash
             (?@ 'clauses.name clauses.impl)
             ...))
         (define module-sig
           (module-signature 'prim
              (hash-union
                (hash (?@ 'clauses.name clauses.ty) ...)
                prim-types)
              (hash)
              (hash)))))]))

(define-primitives
  (supported-primitives primitive-module-signature)

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

  [(a) (panic [bytes : Bytes]) : (type-var a) #:error bytes]

  [(make-bytes [size : Byte]) : Bytes (make-bytes size)]
  [(bytes-ref [b : Bytes] [index : Byte]) : Byte (bytes-ref b index)]
  [(bytes-set! [b : Bytes] [index : Byte] [v : Byte]) : Void (bytes-set! b index v)]
  [(bytes-length [b : Bytes]) : Byte (bytes-length b)]


  [(a) (make-array [size : Byte] [v : (type-var a)]) : (Array a) (make-vector size v)]
  [(a) (array-ref [arr : (Array a)] [index : Byte]) : (type-var a) (vector-ref arr index)]
  [(a) (array-set! [arr : (Array a)] [index : Byte] [v : (type-var a)]) : Void (vector-set! arr index v)]
  [(a) (array-length [arr : (Array a)]) : Byte (vector-length arr)]

  [(open-input-file [name : Bytes]) : InputPort
   (open-input-file (bytes->path (subbytes name 0 (sub1 (bytes-length name)))))]
  [(open-output-file [name : Bytes]) : OutputPort
   (open-output-file (bytes->path (subbytes name 0 (sub1 (bytes-length name)))))]
  [(close-input-port [port : InputPort]) : Void
   (close-input-port port)]
  [(close-output-port [port : OutputPort]) : Void
   (close-output-port port)]

  [(write-bytes [b : Bytes] [p : OutputPort] [start-pos : Byte] [end-pos : Byte]) : Byte
   (write-bytes b p start-pos end-pos)]

  [(read-bytes [b : Bytes] [p : InputPort] [start-pos : Byte] [end-pos : Byte]) : Byte
   (define amount-read (read-bytes! b p start-pos end-pos))
   (if (eof-object? amount-read) 0 amount-read)]

  [(a) (box [v : (type-var a)]) : (Box a) (box v)]
  [(a) (unbox [b : (Box a)]) : (type-var a) (unbox b)]
  [(a) (set-box! [b : (Box a)] [v : (type-var a)]) : Void (set-box! b v)])

