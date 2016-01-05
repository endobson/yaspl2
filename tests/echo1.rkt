(module echo1
  (import
    (prim make-bytes write-bytes read-bytes bytes-ref + - = void))
  (export main)
  (types)

  (define (loop in out size)
    (let ([bytes (make-bytes size)])
      (let ([amount-read (read-bytes bytes in 0 size)])
        (if (= amount-read 0)
            (void)
            (begin
              (write-bytes bytes out 0 amount-read)
              (loop in out (if (= amount-read size) (+ size size) size)))))))


  (define (main stdin stdout stderr)
    (begin
      (loop stdin stdout 1)
      0)))
#:test-cases
(#:module-name echo1 #:stdin #"Hello world" #:stdout #"Hello world")
