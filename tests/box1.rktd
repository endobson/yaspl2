(module box1
  (import
    (prim box unbox set-box! +))
  (export
    #:types ()
    #:values (main)
    #:patterns ())
  (types)

  (define (main [args : (Array Bytes)] [stdin : InputPort] [stdout : OutputPort] [stderr : OutputPort]) : Byte
    (let ([b (box 5)])
      (+ (unbox b)
         (begin
           (set-box! b 6)
           (unbox b))))))
#:test-cases
(#:module-name box1 #:exit-code 11)
