(module box1
  (import
    (prim box unbox set-box! +))
  (export main)
  (types)

  (define (main [stdin : InputPort] [stdout : OutputPort] [stderr : OutputPort]) : Byte
    (let ([b (box 5)])
      (+ (unbox b)
         (begin
           (set-box! b 6)
           (unbox b))))))
#:test-cases
(#:module-name box1 #:exit-code 11)
