#lang racket/base

(require
  file/gzip
  file/gunzip
  racket/format
  racket/list
  racket/match
  racket/port)

(provide
  inflate-bytes*)

(define debug #f)

(define endian-flip-vector (make-vector 256 #f))
(for* ([i0 2] [i1 2] [i2 2] [i3 2] [i4 2] [i5 2] [i6 2] [i7 2])
  (define v1 (vector i0 i1 i2 i3 i4 i5 i6 i7))
  (define v2 (vector i7 i6 i5 i4 i3 i2 i1 i0))
  (define (vector->value v)
    (for/sum ([i 8])
      (* (expt 2 i) (vector-ref v i))))
  (vector-set! endian-flip-vector (vector->value v1) (vector->value v2)))


(define (reverse-string s)
  (list->string (reverse (string->list s))))

(define (inflate-bytes* compressed)
  (define (bytes->bits bytes)
    (append*
      (for/list ([b bytes])
        (reverse (string->list (~r #:pad-string "0" #:base 2 #:min-width 8 b))))))

  (define bits-read 0)

  ;; The number of bytes that haven't yet been put in the buffer
  (define bytes-processed 0)
  (define bits-buffered 0)
  (define bit-buffer 0)

  (define (ensure-buffer-size n)
    (when (< bits-buffered n)
      (define v (bytes-ref compressed bytes-processed))

      (set! bytes-processed (add1 bytes-processed))
      (set! bit-buffer (+ bit-buffer (arithmetic-shift v bits-buffered)))
      (set! bits-buffered (+ 8 bits-buffered))
      (ensure-buffer-size n)))


  (define (peek-little-endian-number n)
    (ensure-buffer-size n)

    (bitwise-and bit-buffer (sub1 (expt 2 n))))

  (define (peek-big-endian-number n)
    (ensure-buffer-size n)

    (cond
      [(<= n 8)
       (arithmetic-shift
         (vector-ref endian-flip-vector (peek-little-endian-number n))
         (- n 8))]
      [(<= n 16)
       (define v (peek-little-endian-number n))
       (define v-low  (bitwise-and v #xff))
       (define v-high (bitwise-and (arithmetic-shift v -8) #xff))
       (define swapped-high (vector-ref endian-flip-vector v-low))
       (define swapped-low (vector-ref endian-flip-vector v-high))
       (arithmetic-shift
         (+ (arithmetic-shift swapped-high 8) swapped-low)
         (- n 16))]
      [else
       (error 'inflate "Bad peek-big-endian-number")]))


  (define (drop-bits! n)
    (set! bits-read (+ bits-read n))
    (set! bits-buffered (- bits-buffered n))
    (set! bit-buffer (arithmetic-shift bit-buffer (- n))))


  (define (get-big-endian-number! n)
    (begin0
      (peek-big-endian-number n)
      (drop-bits! n)))

  (define (get-little-endian-number! n)
    (begin0
      (peek-little-endian-number n)
      (drop-bits! n)))

  (define (get-boolean!)
    (equal? (get-little-endian-number! 1) 1))

  (define (align-bits!)
    (drop-bits! (modulo (- (bitwise-and bits-read 7)) 8)))



  ;; TODO make these peek at the big endian number
  (define (fixed-code!)
    (match (peek-big-endian-number 2)
      [#b11
       (match (peek-big-endian-number 5)
         [#b11000
          (+ (get-big-endian-number! 8) -128 -64 280)]
         [bs
          (+ (get-big-endian-number! 9) -256 -128 -16 144)])]
      [bs
       (match (peek-big-endian-number 4)
         [(or #b0000 #b0001 #b0010)
          (+ (get-big-endian-number! 7) 256)]
         [bs
          (+ (get-big-endian-number! 8) -48)])]))


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
          (get-little-endian-number! n-bits))]))

  (define (fixed-distance!)
    (distance-code->distance (get-big-endian-number! 5)))

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
       (define len (+ base-len (get-little-endian-number! n-bits)))
       (when (= len 258)
         (error 'inflate "Invalid code length"))
       (repeat len (read-distance!))]
      [285
       (repeat 258 (read-distance!))]))

  (define (fixed-code/struct!)
    (code->command (fixed-code!) fixed-distance!))

  (define (read-all-codes read-code)
    (let read-all-codes-loop ()
      (match (read-code)
        [(end)
         empty]
        [code
         (when debug
           (displayln code))
         (cons code (read-all-codes-loop))])))

  (define output-commands empty)

  (let block-loop ()
    (define last-block (get-boolean!))
    (define block-output-commands
      (match (get-little-endian-number! 2)
        [#b00
         (align-bits!)
         (define num-bytes (get-little-endian-number! 16))
         (define num-bytes-complement (get-little-endian-number! 16))
         (unless (equal? #xFFFF (bitwise-xor num-bytes num-bytes-complement))
           (error 'inflate "Lengths don't match"))
         (for/list ([i (in-range num-bytes)])
           (literal (get-little-endian-number! 8)))]
        [#b01
         (read-all-codes fixed-code/struct!)]
        [#b10
         (define num-literals (get-little-endian-number! 5))
         (define num-distance-codes (get-little-endian-number! 5))
         (define num-code-lengths (get-little-endian-number! 4))
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


           (for/list ([len (in-range (vector-length code-vec))]
                      #:unless (empty? (vector-ref code-vec len))
                      )
             (list
               len
               (for*/hash ([code (sort (vector-ref code-vec len) <)])
                 (ensure-length! len)
                 (define prefix (increment-current-code!))
                 (values
                   (string->number (reverse-string prefix) 2)
                   code)))))

         (define (read-using-code-table code-table)
           (let/ec return
             (for ([len-entries (in-list code-table)])
               (match-define (list len entries) len-entries)
               (define prefix (peek-little-endian-number len))
               (match (hash-ref entries prefix #f)
                 [#f (void)]
                 [code-num
                  (drop-bits! len)
                  (return code-num)]))
             (error 'inflate "No prefix code matched")))


         (define code-length-codes
           (list 16 17 18 0 8 7 9 6 10 5 11 4 12 3 13 2 14 1 15))
         (define meta-code-lengths (make-hash))


         (for ([i (+ 4 num-code-lengths)] [code code-length-codes])
           (define code-length (get-little-endian-number! 3))
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
              (define count (+ 3 (get-little-endian-number! 2)))
              (unless prev-code-len
                (error 'inflate "Cannot refer to previous code length on first code."))
              (unless (zero? prev-code-len)
                (for ([i count])
                  (hash-set! encoding-code-lengths (+ num-len-codes-read i) prev-code-len)))
              (increase-num-len-codes! count)]
             [17
              (define count (+ 3 (get-little-endian-number! 3)))
              (set! prev-code-len 0)
              (increase-num-len-codes! count)]
             [18
              (define count (+ 11 (get-little-endian-number! 7)))
              (set! prev-code-len 0)
              (increase-num-len-codes! count)]))

         (define expected-num-codes (+ 258 num-distance-codes num-literals))
         (let process-len-code-loop ()
           (when (< num-len-codes-read expected-num-codes)
             (process-len-code)
             (process-len-code-loop)))
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
    (set! output-commands (append output-commands block-output-commands))
    (unless last-block
      (block-loop)))

  (when debug
    (displayln "Commands:")
    (for ([command output-commands])
      (displayln command)))

  (define full-output (make-bytes 0))
  (define output-buffer (make-bytes (expt 2 15)))
  (define current-output-pos 0)
  (define (increment-buffer-pos!)
    (set! current-output-pos (modulo (add1 current-output-pos)
                                     (bytes-length output-buffer)))
    (when (zero? current-output-pos)
      (set! full-output (bytes-append full-output output-buffer))))


  (for ([output-command output-commands])
    (match output-command
      [(literal val)
       (bytes-set! output-buffer current-output-pos val)
       (increment-buffer-pos!)]
      [(repeat len distance)
       (for ([i (in-range len)])
         (bytes-set! output-buffer current-output-pos
                     (bytes-ref output-buffer
                                (modulo
                                  (- (+ current-output-pos (bytes-length output-buffer)) distance)
                                  (bytes-length output-buffer))))
         (increment-buffer-pos!))]))

  (bytes-append
    full-output
    (subbytes output-buffer 0 current-output-pos)))

