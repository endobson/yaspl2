(module echo2
  (import (prim bytes-length write-bytes)
          (io read-all-bytes)
          )
  (export main)
  (types)

  (define (loop in out size)
    (let ([bytes (make-bytes size)])
      (let ([amount-read (read-bytes bytes in 0 size)])
        (if (= amount-read 0)
            (void)
            (begin
              (write-bytes bytes out 0 amount-read)
              (loop in out (+ size size)))))))


  (define (main stdin stdout stderr)
    (begin
      (let ([bytes (read-all-bytes stdin)])
        (write-bytes bytes stdout 0 (bytes-length bytes)))
      0)))
#:test-cases
(#:module-name echo2 #:stdin #"Hello world" #:stdout #"Hello world")

