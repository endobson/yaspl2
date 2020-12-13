#lang racket/base

(require
  file/gzip
  file/gunzip
  racket/format
  racket/list
  racket/match
  racket/port)

(define debug #f)


(define (reverse-string s)
  (list->string (reverse (string->list s))))

(define (inflate-bytes* compressed)
  (define bits
    (append*
      (for/list ([b compressed])
        (reverse (string->list (~r #:pad-string "0" #:base 2 #:min-width 8 b))))))

  (when debug
    (displayln bits))
  
  (define (peek-bits n)
    (list->string (take bits n)))

  (define (drop-bits! n)
    (when debug
      (printf "Bits: ~a~n" (list->string (take bits n))))
    (set! bits (drop bits n)))

  (define (get-bits! n)
    (begin0
      (peek-bits n)
      (drop-bits! n)))
  
  (define (fixed-code!)
    (match (get-bits! 2)
      ["11"
       (match (get-bits! 3)
         ["000"
          (+ (string->number (get-bits! 3) 2) 280)]
         [bs
          (+ (string->number (string-append bs (get-bits! 4)) 2) -16 144)])]
      [bs
       (match (string-append bs (get-bits! 2))
         [(and bs (or "0000" "0001" "0010"))
          (+ (string->number (string-append bs (get-bits! 3)) 2) 256)]
         [bs
          (+ (string->number (string-append bs (get-bits! 4)) 2) -48)])]))
  
  (define (fixed-distance!)
    (match (string->number (get-bits! 5) 2)
      [(? (lambda (x) (<= 0 x 3)) val)
       (add1 val)]
      [(? (lambda (x) (<= 4 x 29)) val)
       (define-values (n-bits odd) (quotient/remainder (- val 2) 2))
       (when debug
         (printf "Distance: ~a ~a ~a~n" val n-bits odd))
       (+ (expt 2 (add1 n-bits))
          1
          (* odd (expt 2 n-bits))
          (string->number (reverse-string (get-bits! n-bits)) 2))]))
  
  (struct literal (val) #:transparent)
  (struct repeat (length distance) #:transparent)
  (struct end () #:transparent)
  
  (define (fixed-code/struct!)
    (match (fixed-code!)
      [(? (lambda (x) (<= 0 x 255)) val)
       (literal val)]
      [256
       (end)]
      [(? (lambda (x) (<= 257 x 264)) val)
       (define len (+ (* (- val 257) 1) 3))
       (repeat len (fixed-distance!))]
      [(? (lambda (x) (<= 265 x 284)) val)
       (define-values (n-bits extra) (quotient/remainder (- val 261) 4))
       (define base-len (+ 3 (expt 2 (+ 2 n-bits)) (* extra (expt 2 n-bits))))
       (define len (+ base-len (string->number (reverse-string (get-bits! n-bits)) 2)))
       (when (= len 258)
         (error 'inflate "Invalid code length"))
       (repeat len (fixed-distance!))]

      [285
       (repeat 258 (fixed-distance!))]))
  
  
  
  (unless (equal? (get-bits! 1) "1")
    (error 'inflate "Not last block"))


  (define output-commands
    (match (get-bits! 2)
      ["10"
       (let loop ()
         (match (fixed-code/struct!)
           [(end)
            empty]
           [code
            (when debug
              (displayln code))
            (cons code (loop))]))]
      ["01"
       (define num-literals (string->number (reverse-string (get-bits! 5)) 2))
       (define num-distance-codes (string->number (reverse-string (get-bits! 5)) 2))
       (define num-code-lengths (string->number (reverse-string (get-bits! 4)) 2))
       (when debug
         (printf "Literals: 257 + ~a~n" num-literals)
         (printf "Distance codes: 1 + ~a~n" num-distance-codes)
         (printf "Code lengths: 4 + ~a~n" num-code-lengths))
       (define code-length-codes
         (list 16 17 18 0 8 7 9 6 10 5 11 4 12 3 13 2 14 1 15))
       (for ([i (+ 4 num-code-lengths)] [code code-length-codes])
         (define code-length (string->number (reverse-string (get-bits! 3)) 2))
         (when (and debug (not (zero? code-length)))
           (printf "Code ~a is length ~a~n" code code-length)))

       (define num-len-codes-read 0)
       (define (increase-num-len-codes! amt)
         (set! num-len-codes-read (+ num-len-codes-read amt)))

       (define (read-len-code)
         (cond
           [(equal? (peek-bits 2) "00")
            (drop-bits! 2)
            (when debug
              (printf "Code ~a is ~a~n" num-len-codes-read 0))
            (increase-num-len-codes! 1)]
           [(equal? (peek-bits 3) "010")
            (drop-bits! 3)
            (when debug
              (printf "Code ~a is ~a~n" num-len-codes-read 1))
            (increase-num-len-codes! 1)]
           [(equal? (peek-bits 3) "011")
            (drop-bits! 3)
            (when debug
              (printf "Code ~a is ~a~n" num-len-codes-read 2))
            (increase-num-len-codes! 1)]
           [(equal? (peek-bits 3) "100")
            (drop-bits! 3)
            (when debug
              (printf "Code ~a is ~a~n" num-len-codes-read 3))
            (increase-num-len-codes! 1)]
           [(equal? (peek-bits 3) "101")
            (drop-bits! 3)
            (when debug
              (printf "Code ~a is ~a~n" num-len-codes-read 5))
            (increase-num-len-codes! 1)]
           [(equal? (peek-bits 3) "110")
            (drop-bits! 3)
            (when debug
              (printf "Code is ~a~n" 18))
            (define count (+ 11 (string->number (reverse-string (get-bits! 7)) 2)))
            (when debug
              (printf "Extra is ~a~n" count))
            (increase-num-len-codes! count)]
           [(equal? (peek-bits 4) "1110")
            (drop-bits! 4)
            (when debug
              (printf "Code ~a is ~a~n" num-len-codes-read 4))
            (increase-num-len-codes! 1)]
           [(equal? (peek-bits 4) "1111")
            (drop-bits! 4)
            (when debug
              (printf "Code is ~a~n" 17))
            (define count (+ 3 (string->number (reverse-string (get-bits! 3)) 2)))
            (when debug
              (printf "Extra is ~a~n" count))
            (increase-num-len-codes! count)]))
       (read-len-code)
       (read-len-code)
       (read-len-code)
       (read-len-code)
       (read-len-code)
       (read-len-code)
       (read-len-code)
       (read-len-code)
       (read-len-code)
       (read-len-code)
       (read-len-code)
       (read-len-code)
       (read-len-code)
       (read-len-code)
       (read-len-code)
       (read-len-code)
       (read-len-code)
       (when debug
         (printf "NumCodesRead ~a~n" num-len-codes-read))

       (define (read-distance)
         (cond
           [(equal? (peek-bits 1) "0")
            (drop-bits! 1)
            (define extra (string->number (reverse-string (get-bits! 2)) 2))
            (+ 9 extra)]
           [(equal? (peek-bits 1) "1")
            (drop-bits! 1)
            (define extra (string->number (reverse-string (get-bits! 3)) 2))
            (+ 17 extra)]))

       (define (read-actual-code)
         (cond
           [(equal? (peek-bits 2) "00")
            (drop-bits! 2)
            (literal 97)]
           [(equal? (peek-bits 2) "01")
            (drop-bits! 2)
            (literal 98)]
           [(equal? (peek-bits 2) "10")
            (drop-bits! 2)
            (literal 99)]
           [(equal? (peek-bits 3) "110")
            (drop-bits! 3)
            (literal 100)]
           [(equal? (peek-bits 4) "1110")
            (drop-bits! 4)
            (end)]
           [(equal? (peek-bits 5) "11110")
            (drop-bits! 5)
            (repeat 4 (read-distance))]
           [(equal? (peek-bits 5) "11111")
            (drop-bits! 5)
            (repeat 7 (read-distance))]))

       (let loop ()
         (match (read-actual-code)
           [(end)
            empty]
           [code
            (when debug
              (displayln code))
            (cons code (loop))])) ]))

  (when debug
    (displayln "Commands:")
    (for ([command output-commands])
      (displayln command)))
  
  (define output-buffer (make-bytes (expt 2 15)))
  (define current-output-pos 0)
  
  (for ([output-command output-commands])
    (match output-command
      [(literal val)
       (bytes-set! output-buffer current-output-pos val)
       (set! current-output-pos (add1 current-output-pos))]
      [(repeat len distance)
       (for ([i (in-range len)])
         (bytes-set! output-buffer current-output-pos
                     (bytes-ref output-buffer (- current-output-pos distance)))
         (set! current-output-pos (add1 current-output-pos)))]))
  (subbytes output-buffer 0 current-output-pos))

     
(define (deflate-bytes input)
  (call-with-output-bytes 
    (lambda (out)
      (call-with-input-bytes input
        (lambda (in)
          (deflate in out))))))

(define full-exhaustive-length-zero
  (list #""))

(define full-exhaustive-length-one
  (for/list ([i 256])
    (bytes i)))
(define full-exhaustive-length-two
  (for*/list ([i 256] [j 256])
    (bytes i j)))

(define exhaustive-length-10-alphabet-2
  (let ([symbols
         (let loop ([len 10])
           (if (zero? len)
               (list empty)
               (let ([rec (loop (sub1 len))])
                 (append (map (lambda (l) (cons 65 l)) rec)
                         (map (lambda (l) (cons 66 l)) rec)))))])
    (for/list ([symbol-list symbols])
      (apply bytes symbol-list))))

(define (single-random length alphabet-size)
  (define alphabet (build-vector alphabet-size (lambda (x) (+ x 65))))
  (define b (make-bytes length))
  (for ([i (in-range length)])
    (bytes-set! b i (vector-ref alphabet (random alphabet-size))))
  b)

(define (many-random #:amount amount #:length length #:alphabet-size alphabet-size
                     #:seed [seed 0])
  (parameterize ([current-pseudo-random-generator (make-pseudo-random-generator)])
    (random-seed seed)
    (for/list ([i (in-range amount)])
      (single-random length alphabet-size))))

  
(define test-inputs
  (append
    ;full-exhaustive-length-zero
    ;full-exhaustive-length-one
    ;full-exhaustive-length-two
    ;exhaustive-length-10-alphabet-2
    ;(many-random #:amount 1000 #:length 17 #:alphabet-size 5)
    ;(many-random #:amount 1000 #:length 16 #:alphabet-size 4)
    ;(many-random #:amount 5000 #:length 30 #:alphabet-size 26)

    (list
      #"aababcbacdbcacbcdaababcbaaccddbbccaaccbbd")
    ;(many-random #:amount 1000 #:length 17 #:alphabet-size 5)

    ;(list
    ;  (make-bytes 76 65)
    ;  (make-bytes 200 65)
    ;  #"AAAAABAAAA"
    ;  #"EEDACCBCCDCDDAC")

    ))

(for ([input test-inputs])
  (when debug
    (write input)
    (newline))
  (define compressed (deflate-bytes input))
  (define decompressed (inflate-bytes* compressed))
  (unless (equal? input decompressed)
    (error 'inflate "Input and Decompressed output don't match: ~s ~s" input decompressed)))
