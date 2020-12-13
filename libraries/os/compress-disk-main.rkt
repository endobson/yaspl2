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


  (define (distance-code->distance code)
    (match code
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

  (define (fixed-distance!)
    (distance-code->distance (string->number (get-bits! 5) 2)))

  (struct literal (val) #:transparent)
  (struct repeat (length distance) #:transparent)
  (struct end () #:transparent)

  (define (code->command code read-distance!)
    (match code
      [(? (lambda (x) (<= 0 x 255)) val)
       (literal val)]
      [256
       (end)]
      [(? (lambda (x) (<= 257 x 264)) val)
       (define len (+ (* (- val 257) 1) 3))
       (repeat len (read-distance!))]
      [(? (lambda (x) (<= 265 x 284)) val)
       (define-values (n-bits extra) (quotient/remainder (- val 261) 4))
       (define base-len (+ 3 (expt 2 (+ 2 n-bits)) (* extra (expt 2 n-bits))))
       (define len (+ base-len (string->number (reverse-string (get-bits! n-bits)) 2)))
       (when (= len 258)
         (error 'inflate "Invalid code length"))
       (repeat len (read-distance!))]
      [285
       (repeat 258 (read-distance!))]))

  (define (fixed-code/struct!)
    (code->command (fixed-code!) fixed-distance!))


  (unless (equal? (get-bits! 1) "1")
    (error 'inflate "Not last block"))

  (define (read-all-codes read-code)
    (let loop ()
      (match (read-code)
        [(end)
         empty]
        [code
         (when debug
           (displayln code))
         (cons code (loop))])))


  (define output-commands
    (match (get-bits! 2)
      ["10"
       (read-all-codes fixed-code/struct!)]
      ["01"
       (define num-literals (string->number (reverse-string (get-bits! 5)) 2))
       (define num-distance-codes (string->number (reverse-string (get-bits! 5)) 2))
       (define num-code-lengths (string->number (reverse-string (get-bits! 4)) 2))
       (when debug
         (printf "Literals: 257 + ~a~n" num-literals)
         (printf "Distance codes: 1 + ~a~n" num-distance-codes)
         (printf "Code lengths: 4 + ~a~n" num-code-lengths))

       (define (build-code-table code-vec)
         ;; The current code in little endian order
         (define current-code empty)
         (define (ensure-length! len)
           (define extra (- len (length current-code)))
           (when (not (zero? extra))
             (set! current-code (append (make-list extra #\0) current-code))))
         (define (increment-code code)
           (match code
             [(list)
              ;; If this happens the next code should not be used
              (list)]
             [(cons #\0 code)
              (cons #\1 code)]
             [(cons #\1 code)
              (cons #\0 (increment-code code))]))
         (define (increment-current-code!)
           (begin0
             (list->string (reverse current-code))
             (set! current-code (increment-code current-code))))


         (for*/list ([len (in-range (vector-length code-vec))]
                     [code (sort (vector-ref code-vec len) <)])
           (ensure-length! len)
           (list (increment-current-code!) code)))

       (define (read-using-code-table code-table)
         (let/ec return
           (for ([p (in-list code-table)])
             (match p
               [(list prefix code-num)
                (define n (string-length prefix))
                (when (equal? (peek-bits n) prefix)
                  (drop-bits! n)
                  (return code-num))]))
           (error 'inflate "No prefix code matched")))


       (define code-length-codes
         (list 16 17 18 0 8 7 9 6 10 5 11 4 12 3 13 2 14 1 15))
       (define meta-code-lengths (make-hash))


       (for ([i (+ 4 num-code-lengths)] [code code-length-codes])
         (define code-length (string->number (reverse-string (get-bits! 3)) 2))
         (when (not (zero? code-length))
           (hash-set! meta-code-lengths code code-length)
           (when debug
             (printf "Code ~a is length ~a~n" code code-length))))

       (define max-meta-length
         (apply max (hash-values meta-code-lengths)))


       (define meta-code-vec (make-vector (add1 max-meta-length) empty))
       (for ([(code-num length) meta-code-lengths])
         (vector-set! meta-code-vec length
                      (cons code-num (vector-ref meta-code-vec length))))
       (define meta-code-table (build-code-table meta-code-vec))


       (define num-len-codes-read 0)
       (define (increase-num-len-codes! amt)
         (set! num-len-codes-read (+ num-len-codes-read amt)))

       (define encoding-code-lengths (make-hash))
       (define prev-code-len #f)

       (define (process-len-code)
         (match (read-using-code-table meta-code-table)
           [(? (lambda (x) (<= 0 x 15)) len)
            (unless (zero? len)
              (hash-set! encoding-code-lengths num-len-codes-read len))
            (set! prev-code-len len)
            (increase-num-len-codes! 1)]
           [16
            (define count (+ 3 (string->number (reverse-string (get-bits! 2)) 2)))
            (unless prev-code-len
              (error 'inflate "Cannot refer to previous code length on first code."))
            (unless (zero? prev-code-len)
              (for ([i count])
                (hash-set! encoding-code-lengths (+ num-len-codes-read i) prev-code-len)))
            (increase-num-len-codes! count)]
           [17
            (define count (+ 3 (string->number (reverse-string (get-bits! 3)) 2)))
            (set! prev-code-len 0)
            (increase-num-len-codes! count)]
           [18
            (define count (+ 11 (string->number (reverse-string (get-bits! 7)) 2)))
            (set! prev-code-len 0)
            (increase-num-len-codes! count)]))

       (define expected-num-codes (+ 258 num-distance-codes num-literals))
       (let loop ()
         (when (< num-len-codes-read expected-num-codes)
           (process-len-code)
           (loop)))
       (unless (= num-len-codes-read expected-num-codes)
         (error 'inflate "Too many length codes ~a" num-len-codes-read))


       (define max-litlen-length
         (apply max
           (for/list ([(code-num length) encoding-code-lengths]
                      #:when (< code-num (+ 257 num-literals)))
             length)))
       (define max-distance-length
         (apply max
           (for/list ([(code-num length) encoding-code-lengths]
                      #:when (>= code-num (+ 257 num-literals)))
             length)))

       (define litlen-code-vec (make-vector (add1 max-litlen-length) empty))
       (for ([(code-num length) encoding-code-lengths]
             #:when (< code-num (+ 257 num-literals)))
         (vector-set! litlen-code-vec length
                      (cons code-num (vector-ref litlen-code-vec length))))

       (define distance-code-vec (make-vector (add1 max-distance-length) empty))
       (for ([(code-num length) encoding-code-lengths]
             #:when (>= code-num (+ 257 num-literals)))
         (vector-set! distance-code-vec length
                      (cons (- code-num (+ 257 num-literals))
                            (vector-ref distance-code-vec length))))



       (define litlen-code-table (build-code-table litlen-code-vec))
       (define distance-code-table (build-code-table distance-code-vec))

       (define (read-distance)
         (distance-code->distance (read-using-code-table distance-code-table)))

       (define (read-actual-command)
         (code->command (read-using-code-table litlen-code-table) read-distance))

       (read-all-codes read-actual-command)]))

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
    ;(many-random #:amount 5000 #:length 30 #:alphabet-size 26)
    ;(many-random #:amount 5000 #:length 100 #:alphabet-size 5)


    (list
      (make-bytes 76 65)
      (make-bytes 200 65)
      #"AAAAABAAAA"
      #"EEDACCBCCDCDDAC"
      #"aababcbacdbcacbcdaababcbaaccddbbccaaccbbd"
      #"aababcbabccdacdbcacbcdaabbbbbbaacdbcbaaccddbbccaaccbbd")

    ))

(for ([input test-inputs])
  (when debug
    (write input)
    (newline))
  (define compressed (deflate-bytes input))
  (define decompressed (inflate-bytes* compressed))
  (unless (equal? input decompressed)
    (error 'inflate "Input and Decompressed output don't match: ~s ~s" input decompressed)))
