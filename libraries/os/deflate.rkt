#lang racket/base

(require
  racket/list
  racket/match
  racket/port
  file/gunzip
  racket/fixnum)

(define (fxsub1 n)
  (fx- n 1))

(define endian-flip-vector (make-vector 256 #f))
(for* ([i0 2] [i1 2] [i2 2] [i3 2] [i4 2] [i5 2] [i6 2] [i7 2])
  (define v1 (vector i0 i1 i2 i3 i4 i5 i6 i7))
  (define v2 (vector i7 i6 i5 i4 i3 i2 i1 i0))
  (define (vector->value v)
    (for/sum ([i 8])
      (* (expt 2 i) (vector-ref v i))))
  (vector-set! endian-flip-vector (vector->value v1) (vector->value v2)))


(define (output-bit-stream output-port)
  (define bits-buffered 0)
  (define bit-buffer 0)

  (define (write-little-endian-number x n)
    (define new-bits (fxand x (fxsub1 (fxlshift 2 n))))
    (set! bit-buffer (fxior bit-buffer (fxlshift new-bits bits-buffered)))
    (set! bits-buffered (fx+ bits-buffered n))
    (maybe-flush-bits))

  (define (maybe-flush-bits)
    (when (fx>= bits-buffered 8)
      (flush-byte)
      (maybe-flush-bits)))

  (define (flush-byte)
    (write-byte (fxand bit-buffer #xff) output-port)
    (set! bit-buffer (fxrshift bit-buffer 8))
    (set! bits-buffered (fxmax 0 (fx- bits-buffered 8))))


  (values
    write-little-endian-number
    flush-byte))


(define b
  (call-with-output-bytes
    (lambda (p)
      (define-values (write-little-endian-number flush-byte) (output-bit-stream p))
      (write-little-endian-number 7 4)
      (write-little-endian-number 2 4)
      (write-little-endian-number 6 4)
      (flush-byte))))

(struct var-code (code extra-bits extra-n) #:transparent)

(define (length-code->num-extra-bits code)
  (cond
    [(fx<= code 256)
     (error 'length-code->num-extra-bits "Not a length code")]
    [(fx<= code 264) 0]
    [(fx<= code 284) (fxrshift (fx- code 261) 4)]
    [(fx= 285) 0]))
(define (length-code->base code)
  (cond
    [(fx<= code 256)
     (error 'length-code->base "Not a length code")]
    [(fx<= code 264) (fx- code 254)]
    [(fx<= code 268) (fx+ 11 (fxlshift (fx- code 265) 1))]
    [(fx<= code 272) (fx+ 11 8 (fxlshift (fx- code 269) 2))]
    [(fx<= code 276) (fx+ 11 8 16 (fxlshift (fx- code 273) 3))]
    [(fx<= code 280) (fx+ 11 8 16 32 (fxlshift (fx- code 277) 4))]
    [(fx<= code 284) (fx+ 11 8 16 32 64 (fxlshift (fx- code 281) 5))]
    [(fx= 285) 258]))


(define (length->var-code len)
  (cond
    [(fx<= len 2)
     (error 'length->var-code "Cannot encode length less than 3")]
    [(fx<= len 10)
     (var-code (fx+ 254 len) 0 0)]
    [(fx<= len 18)
     (define extra (fx- len 3 8))
     (define high-extra (fxrshift extra 1))
     (define low-extra (fxand extra #b1))
     (var-code (fx+ 265 high-extra) low-extra 1)]
    [(fx<= len 34)
     (define extra (fx- len 3 8 8))
     (define high-extra (fxrshift extra 2))
     (define low-extra (fxand extra #b11))
     (var-code (fx+ 269 high-extra) low-extra 2)]
    [(fx<= len 66)
     (define extra (fx- len 3 8 8 16))
     (define high-extra (fxrshift extra 3))
     (define low-extra (fxand extra #b111))
     (var-code (fx+ 273 high-extra) low-extra 3)]
    [(fx<= len 130)
     (define extra (fx- len 3 8 8 16 32))
     (define high-extra (fxrshift extra 4))
     (define low-extra (fxand extra #b1111))
     (var-code (fx+ 277 high-extra) low-extra 4)]
    [(fx<= len 257)
     (define extra (fx- len 3 8 8 16 32 64))
     (define high-extra (fxrshift extra 5))
     (define low-extra (fxand extra #b11111))
     (var-code (fx+ 281 high-extra) low-extra 5)]
    [(fx= len 258)
     (var-code 285 0 0)]
    [else
     (error 'length->var-code "Cannot encode lengths greater than 258")]))

(define (var-code->length vcode)
  (match-define (var-code code extra _) vcode)
  (+ (length-code->base code) extra))



(length->var-code 4)
(length->var-code 11)
(length->var-code 12)
(length->var-code 17)
(length->var-code 25)

(length->var-code 47)
(length->var-code 257)
(length->var-code 258)

(for ([i (in-range 3 259)])
  (define vcode (length->var-code i))
  (unless (fx= (var-code->length vcode) i)
    (error 'length->var-code "Bad conversion")))


(struct literal (v))
(struct repeat (length distance))

(define (deflate* input-port output-port)

  (define-values (write-little-endian-number flush-byte)
    (output-bit-stream output-port))

  (define (write-big-endian-number v n)
    (unless (fx<= n 8)
      (error 'inflate "Bad peek-big-endian-number"))

    (write-little-endian-number
      (fxrshift
        (vector-ref endian-flip-vector v)
        (fx- 8 n))
      n))

  (define (write-fixed-literal v)
    (cond
      [(fx<= 0 v 143)
       (write-big-endian-number (fx+ v 48) 8)]
      [(fx<= 144 v 255)
       (write-little-endian-number 1 1)
       (write-big-endian-number v 8)]))
  (define (write-fixed-end-block)
    (write-big-endian-number 0 7))

  ;(define (write-fixed-code v)
  ;  (match v
  ;    [(literal (? (lambda (v) (fx<= 0 v 143)) v))
  ;     (w
  ;     (write-little-endian-number (litlen-table v))]
  ;    [(repeat length distance)
  ;     (match-define (var-code len-code len-extra len-extra-n) (length->var-code length))
  ;     (match-define (var-code dist-code dist-extra dist-extra-n) (length->var-code length))
  ;     (define-values (len-prefix len-prefix-n) (litlen-table len-code))
  ;     (write-little-endian-number len-prefix len-prefix-n)
  ;     (write-little-endian-number len-extra len-extra-n)
  ;     (define-values (dist-prefix dist-prefix-n) (distance-table dist-code))
  ;     (write-little-endian-number dist-prefix dist-prefix-n)
  ;     (write-little-endian-number dist-extra dist-extra-n)]))


  ;(define (write-dynamic-code litlen-table distance-table v bit-stream)
  ;  (match v
  ;    [(literal v)
  ;     (write-little-endian-number (litlen-table v))]
  ;    [(repeat length distance)
  ;     (match-define (var-code len-code len-extra len-extra-n) (length->var-code length))
  ;     (match-define (var-code dist-code dist-extra dist-extra-n) (length->var-code length))
  ;     (define-values (len-prefix len-prefix-n) (litlen-table len-code))
  ;     (write-little-endian-number len-prefix len-prefix-n)
  ;     (write-little-endian-number len-extra len-extra-n)
  ;     (define-values (dist-prefix dist-prefix-n) (distance-table dist-code))
  ;     (write-little-endian-number dist-prefix dist-prefix-n)
  ;     (write-little-endian-number dist-extra dist-extra-n)]))

  (write-little-endian-number 1 1)
  (write-little-endian-number 1 2)
  (let loop ()
    (define b (read-byte input-port))
    (unless (eof-object? b)
      (write-fixed-literal b)
      (loop)))

  (write-fixed-end-block)
  (flush-byte))



(define (inflate-bytes input)
  (call-with-output-bytes
    (lambda (out)
      (call-with-input-bytes input
        (lambda (in)
          (inflate in out))))))

(define (deflate-bytes* input)
  (call-with-output-bytes
    (lambda (out)
      (call-with-input-bytes input
        (lambda (in)
          (deflate* in out))))))

(define (round-trip b)
  (inflate-bytes (deflate-bytes* b)))

(define compressed
  (call-with-output-bytes
    (lambda (output-port)
      (call-with-input-bytes #"abcd"
        (lambda (input-port)
          (deflate* input-port output-port))))))



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

(define (single-uncompressable length)
  (define b (make-bytes length))
  (for ([i (in-range length)])
    (bytes-set! b i (random 255)))
  b)

(define (many-uncompressable #:amount amount #:length length
                             #:seed [seed 0])
  (parameterize ([current-pseudo-random-generator (make-pseudo-random-generator)])
    (random-seed seed)
    (for/list ([i (in-range amount)])
      (single-uncompressable length))))

(define test-inputs
  (append
    full-exhaustive-length-zero
    full-exhaustive-length-one
    exhaustive-length-10-alphabet-2
    (many-random #:amount 1000 #:length 17 #:alphabet-size 5)
    (many-random #:amount 1000 #:length 30 #:alphabet-size 26)
    (many-random #:amount 1000 #:length 100 #:alphabet-size 5)
    (many-uncompressable #:amount 10 #:length 100)

    ))

(for ([input test-inputs])
  (define compressed (deflate-bytes* input))
  (define decompressed (inflate-bytes compressed))
  (unless (equal? input decompressed)
    (error 'deflate "Input and Decompressed output don't match: ~s ~s" input decompressed)))



;(struct -bl


