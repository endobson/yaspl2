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
      #:with ty #'(int-ty))
    (pattern (~datum Int)
      #:with ty #'(int-ty))
    (pattern (~datum U8)
      #:with ty #'(u8-ty))
    (pattern (~datum S32)
      #:with ty #'(s32-ty))
    (pattern (~datum U32)
      #:with ty #'(u32-ty))
    (pattern (~datum S64)
      #:with ty #'(int-ty))
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
    'U8 (u8-ty)
    'S32 (s32-ty)
    'U32 (u32-ty)
    'Byte (int-ty)
    'Int (int-ty)
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


  [(u8 [v : S64]) : U8
   (if (<= 0 v #xFF) v (error 'u8))]
  [(u8->s64 [v : U8]) : S64 v]

  [(s32 [v : S64]) : S32
   (if (<= #x-80000000 v #x7FFFFFFF) v (error 's32 "Value ~s is outside valid range" v))]
  [(u32 [v : S64]) : U32
   (if (<= 0 v #xFFFFFFFF) v (error 'u32 "Value ~s is outside valid range" v))]
  [(s32->s64 [v : S32]) : S64 v]
  [(u32->s64 [v : U32]) : S64 v]

  [(u32/le-byte0 [v : U32]) : U8
   (bitwise-and v #xFF)]
  [(u32/le-byte1 [v : U32]) : U8
   (bitwise-and (arithmetic-shift v #x-8) #xFF)]
  [(u32/le-byte2 [v : U32]) : U8
   (bitwise-and (arithmetic-shift v #x-10) #xFF)]
  [(u32/le-byte3 [v : U32]) : U8
   (bitwise-and (arithmetic-shift v #x-18) #xFF)]

  [(s32/le-byte0 [v : S32]) : U8
   (bitwise-and v #xFF)]
  [(s32/le-byte1 [v : S32]) : U8
   (bitwise-and (arithmetic-shift v #x-8) #xFF)]
  [(s32/le-byte2 [v : S32]) : U8
   (bitwise-and (arithmetic-shift v #x-10) #xFF)]
  [(s32/le-byte3 [v : S32]) : U8
   (bitwise-and (arithmetic-shift v #x-18) #xFF)]



  ;; TODO handle overflow/underflow
  [(+ [x : Byte] (y : Byte)) : Byte (+ x y)]
  [(- [x : Byte] (y : Byte)) : Byte (- x y)]
  [(* [x : Byte] (y : Byte)) : Byte (* x y)]
  [(quotient [x : Byte] (y : Byte)) : Byte (quotient x y)]
  [(remainder [x : Byte] (y : Byte)) : Byte (remainder x y)]
  [(bitwise-and [x : Byte] (y : Byte)) : Byte (bitwise-and x y)]
  [(bitwise-ior [x : Byte] (y : Byte)) : Byte (bitwise-ior x y)]
  [(logical-shift-left [x : Byte] (y : Byte)) : Byte (arithmetic-shift x y)]
  ;; TODO (make this not arithmetic shift)
  [(logical-shift-right [x : Byte] (y : Byte)) : Byte (arithmetic-shift x (- y))]

  [(= [x : Byte] (y : Byte)) : Boolean (= x y)]
  [(<= [x : Byte] (y : Byte)) : Boolean (<= x y)]
  [(>= [x : Byte] (y : Byte)) : Boolean (>= x y)]
  [(< [x : Byte] (y : Byte)) : Boolean (< x y)]
  [(> [x : Byte] (y : Byte)) : Boolean (> x y)]

  [(void) : Void (void)]

  [(a) (panic [bytes : Bytes]) : (type-var a) #:error bytes]

  [(make-bytes [size : Byte]) : Bytes (make-bytes size)]
  [(bytes-ref [b : Bytes] [index : Byte]) : Byte (bytes-ref b index)]
  [(bytes-set! [b : Bytes] [index : Byte] [v : U8]) : Void (bytes-set! b index v)]
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
  [(delete-file [name : Bytes]) : Void
   (delete-file (bytes->path (subbytes name 0 (sub1 (bytes-length name)))))]
  [(set-file-or-directory-permissions [name : Bytes] [permissions : Int]) : Void
   (file-or-directory-permissions (bytes->path (subbytes name 0 (sub1 (bytes-length name))))
                                  permissions)]

  [(write-bytes [b : Bytes] [p : OutputPort] [start-pos : Byte] [end-pos : Byte]) : Byte
   (write-bytes b p start-pos end-pos)]

  [(read-bytes [b : Bytes] [p : InputPort] [start-pos : Byte] [end-pos : Byte]) : Byte
   (define amount-read (read-bytes! b p start-pos end-pos))
   (if (eof-object? amount-read) 0 amount-read)]

  [(a) (box [v : (type-var a)]) : (Box a) (box v)]
  [(a) (unbox [b : (Box a)]) : (type-var a) (unbox b)]
  [(a) (set-box! [b : (Box a)] [v : (type-var a)]) : Void (set-box! b v)])

