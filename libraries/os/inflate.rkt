#lang racket/base

(require
  file/gzip
  file/gunzip
  racket/format
  racket/list
  racket/match
  racket/port
  racket/require
  racket/performance-hint
  "util.rkt"
  (for-syntax
    racket/base)
  #;
  racket/fixnum
  (filtered-in
    (lambda (name)
      (and (regexp-match #rx"^unsafe-fx" name)
           (regexp-replace #rx"unsafe-" name "")))
	racket/unsafe/ops)
  (only-in racket/unsafe/ops
    [unsafe-bytes-ref bytes-ref]
    [unsafe-bytes-length bytes-length]
    [unsafe-bytes-copy! bytes-copy!]
    [unsafe-bytes-set! bytes-set!]
    [unsafe-vector-set! vector-set!]
    [unsafe-vector-ref vector-ref])
  )


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

(define (expt2 n)
  (fxlshift 1 n))
(define (fxsub1 n)
  (fx- n 1))
(define (fxadd1 n)
  (fx+ n 1))

(define (output-buffer full-output)
  (define output-buffer* (make-bytes (expt 2 15)))
  (define current-output-pos 0)

  (define (increment-buffer-pos! x)
    (set! current-output-pos (fxmodulo (fx+ current-output-pos x)
                                       (bytes-length output-buffer*)))
    (when (zero? current-output-pos)
      (write-all-bytes output-buffer* full-output)))

  (define (handle-literal val)
    (bytes-set! output-buffer* current-output-pos val)
    (increment-buffer-pos! 1))

  (define (handle-repeat len distance)
    (unless (fx= 0 len)
      (define raw-start (fx- current-output-pos distance))
      (define-values (start copy-len)
        (if (fx< raw-start 0)
            (values (fx+ (bytes-length output-buffer*) raw-start)
                    (fxmin len (fx- raw-start)))
            (values raw-start (fxmin len 
                                     distance
                                     (fx- (bytes-length output-buffer*) current-output-pos)))))

      (bytes-copy! output-buffer* current-output-pos
                   output-buffer* start (fx+ start copy-len))
          

      (increment-buffer-pos! copy-len)
      (handle-repeat (fx- len copy-len) distance)))

  (define (flush-output)
    (write-all-bytes
      (subbytes output-buffer* 0 current-output-pos)
      full-output))

  (values increment-buffer-pos!
          handle-literal
          handle-repeat
          flush-output))

(define-inline (input-bit-stream input-port)
  (define byte-buffer (make-bytes 4096))
  (define bytes-buffered 0)
  (define bytes-read 0)

  (define bits-buffered 0)
  (define bit-buffer 0)

  (define (ensure-buffer-size n)
    (when (fx< bits-buffered n)
      (when (fx= bytes-read bytes-buffered)
        (set! bytes-buffered (read-bytes! byte-buffer input-port))
        (set! bytes-read 0))

      (define v (bytes-ref byte-buffer bytes-read))
      (set! bytes-read (fxadd1 bytes-read))

      (set! bit-buffer (fx+ bit-buffer (fxlshift v bits-buffered)))
      (set! bits-buffered (fx+ 8 bits-buffered))
      (ensure-buffer-size n)))


  (define (peek-little-endian-number n)
    (ensure-buffer-size n)

    (fxand bit-buffer (fxsub1 (expt2 n))))

  (define (drop-bits! n)
    (set! bits-buffered (fx- bits-buffered n))
    (set! bit-buffer (fxrshift bit-buffer n)))

  (define (align-bits!)
    (drop-bits! (fxmodulo bits-buffered 8)))


  (values
    peek-little-endian-number
    drop-bits!
    align-bits!))




(define (inflate-bytes* compressed)
  (define input (open-input-bytes compressed))

  (define-values (peek-little-endian-number
                  drop-bits!
                  align-bits!)
    (input-bit-stream input))

  (define (peek-big-endian-number n)
    (unless (fx<= n 8)
      (error 'inflate "Bad peek-big-endian-number"))

    (fxrshift
      (vector-ref endian-flip-vector (peek-little-endian-number n))
      (fx- 8 n)))

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


  ;; Output functions
  (define full-output (open-output-bytes))

  (define-values (increment-buffer-pos!
                  handle-literal
                  handle-repeat
                  flush-output)
    (output-buffer full-output))


  (define (fixed-code!)
    (match (peek-big-endian-number 2)
      [#b11
       (match (peek-big-endian-number 5)
         [#b11000
          (fx+ (get-big-endian-number! 8) -128 -64 280)]
         [bs
          ;; Drop the first known bit so that we only need to read
          ;; an 8 bit big endian number.
          (drop-bits! 1)
          (fx+ (get-big-endian-number! 8) -128 -16 144)])]
      [bs
       (match (peek-big-endian-number 4)
         [(or #b0000 #b0001 #b0010)
          (fx+ (get-big-endian-number! 7) 256)]
         [bs
          (fx+ (get-big-endian-number! 8) -48)])]))


  (define (distance-code->distance code)
    (match code
      [(? (lambda (x) (fx<= 0 x 3)) val)
       (fxadd1 val)]
      [(? (lambda (x) (fx<= 4 x 29)) val)
       (define n-bits (fxquotient (fx- val 2) 2))
       (define odd (fxremainder (fx- val 2) 2))
       (fx+ (expt2 (fxadd1 n-bits))
            1
            (fx* odd (expt2 n-bits))
            (get-little-endian-number! n-bits))]))

  (define (fixed-distance!)
    (distance-code->distance (get-big-endian-number! 5)))

  (define (handle-litlen-code code read-distance!)
    (cond
      [(fx<= code 255)
       (handle-literal code)]
      [(fx= code 256)
       #f]
      [(fx<= code 264)
       (define len (fx+ (fx* (fx- code 257) 1) 3))
       (handle-repeat len (read-distance!))]
      [(fx<= code 284)
       (define n-bits (fxquotient (fx- code 261) 4))
       (define extra (fxremainder (fx- code 261) 4))
       (define base-len (fx+ 3 (fxlshift (fx+ 4 extra) n-bits)))
       (define len (fx+ base-len (get-little-endian-number! n-bits)))
       (when (fx= len 258)
         (error 'inflate "Invalid code length"))
       (handle-repeat len (read-distance!))]
      [(fx= 285)
       (handle-repeat 258 (read-distance!))]))

  (define (fixed-code/struct!)
    (handle-litlen-code (fixed-code!) fixed-distance!))

  (define-inline (read-all-codes read-code)
    (let read-all-codes-loop ()
      (when (read-code)
        (read-all-codes-loop))))

  (define (process-literal-block)
    (align-bits!)
    (define num-bytes (get-little-endian-number! 16))
    (define num-bytes-complement (get-little-endian-number! 16))
    (unless (equal? #xFFFF (bitwise-xor num-bytes num-bytes-complement))
      (error 'inflate "Lengths don't match"))
    (for/list ([i (in-range num-bytes)])
      (handle-literal (get-little-endian-number! 8))))

  (define (process-fixed-block)
    (read-all-codes fixed-code/struct!))

  (define (process-dynamic-block)
    (define num-literals (get-little-endian-number! 5))
    (define num-distance-codes (get-little-endian-number! 5))
    (define num-code-lengths (get-little-endian-number! 4))

    (define (build-code-table code-vec type)
      ;; The current code in little endian order
      (define current-code empty)
      (define (ensure-length! len)
        (define extra (fx- len (length current-code)))
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


      (define tables
        (for/list ([len (in-range (vector-length code-vec))]
                   #:unless (empty? (vector-ref code-vec len)))
          (define hash-table
            (for*/hash ([code (sort (vector-ref code-vec len) <)])
              (ensure-length! len)
              (define prefix (increment-current-code!))
              (values
                (string->number (reverse-string prefix) 2)
                code)))
          (define val
            (if (<= len 10)
                (let ([vec (make-vector (expt2 len) #f)])
                  (for ([(k v) hash-table])
                    (vector-set! vec k v))
                  vec)
                hash-table))

          (list len val)))

      (define optimizable?
        (or (and (equal? type 'litlen)
                 (<= (vector-length code-vec) 10)
                 (member 256 (vector-ref code-vec (sub1 (vector-length code-vec)))))
            (and (equal? type 'distance)
                 (<= (vector-length code-vec) 10))))


      (define optimized
        (and optimizable?
          (let ()
            (define max-length (sub1 (vector-length code-vec)))
            (define full-vector (make-vector (expt2 max-length) #f))
            (for ([pair tables])
              (match-define (list len entries) pair)
              (for ([code (in-vector entries)] [prefix (in-naturals)])
                (when code
                  (for ([i (expt2 (- max-length len))])
                    (vector-set! full-vector 
                                 (+ prefix (arithmetic-shift i len))
                                 (list code len))))))
            (lambda ()
              (define prefix (peek-little-endian-number max-length))
              (match-define (list code len) (vector-ref full-vector prefix))
              (drop-bits! len)
              code))))

      (or
        optimized
        (lambda ()
          (let loop ([all-entries tables])
            (match all-entries
              [(cons (list len entries) all-entries)
               (define prefix (peek-little-endian-number len))
               (match (if (vector? entries)
                          (vector-ref entries prefix)
                          (hash-ref entries prefix #f))
                 [#f (loop all-entries)]
                 [code-num
                  (drop-bits! len)
                  code-num])]
              [(list) (error 'inflate "No prefix code matched")])))))



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
    (define meta-code-table (build-code-table meta-code-vec 'meta))


    (define num-len-codes-read 0)
    (define (increase-num-len-codes! amt)
      (set! num-len-codes-read (+ num-len-codes-read amt)))

    (define encoding-code-lengths (make-hash))
    (define prev-code-len #f)

    (define (process-len-code)
      (match (meta-code-table)
        [(? (lambda (x) (<= 0 x 15)) len)
         (unless (zero? len)
           (hash-set! encoding-code-lengths num-len-codes-read len))
         (set! prev-code-len len)
         (increase-num-len-codes! 1)]
        [16
         (define count (fx+ 3 (get-little-endian-number! 2)))
         (unless prev-code-len
           (error 'inflate "Cannot refer to previous code length on first code."))
         (unless (zero? prev-code-len)
           (for ([i count])
             (hash-set! encoding-code-lengths (+ num-len-codes-read i) prev-code-len)))
         (increase-num-len-codes! count)]
        [17
         (define count (fx+ 3 (get-little-endian-number! 3)))
         (set! prev-code-len 0)
         (increase-num-len-codes! count)]
        [18
         (define count (fx+ 11 (get-little-endian-number! 7)))
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

    (define litlen-code-table (build-code-table litlen-code-vec 'litlen))
    (define distance-code-table (build-code-table distance-code-vec 'distance))


    (define (read-distance)
      (distance-code->distance (distance-code-table)))

    (define (read-actual-command)
      (handle-litlen-code (litlen-code-table) read-distance))

    (read-all-codes read-actual-command))

  (let block-loop ()
    (define last-block (get-boolean!))
    (match (get-little-endian-number! 2)
      [#b00 (process-literal-block)]
      [#b01 (process-fixed-block)]
      [#b10 (process-dynamic-block)])
    (unless last-block
      (block-loop)))
  (flush-output)

  (get-output-bytes full-output))

