(module lambda1
  (import)
  (export
    #:types ()
    #:values (main)
    #:patterns ())
  (types)
  (define (main [args : (Array Bytes)] [stdin : InputPort] [stdout : OutputPort] [stderr : OutputPort]) : Byte
    (let ([v (lambda ([x : Byte]) 1)])
      (v 2))))
#:test-cases
(#:module-name lambda1 #:exit-code 1)
