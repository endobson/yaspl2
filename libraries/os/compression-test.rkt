#lang racket/base

(require
  file/gzip
  file/gunzip
;  profile
  racket/format
  racket/list
  racket/match
  racket/port
  "compress-disk-main.rkt")

(define (deflate-bytes input)
  (call-with-output-bytes
    (lambda (out)
      (call-with-input-bytes input
        (lambda (in)
          (deflate in out))))))

(define (inflate-bytes input)
  (call-with-output-bytes
    (lambda (out)
      (call-with-input-bytes input
        (lambda (in)
          (inflate in out))))))

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
    ;full-exhaustive-length-zero
    ;full-exhaustive-length-one
    ;full-exhaustive-length-two
    ;exhaustive-length-10-alphabet-2
    (many-random #:amount 1000 #:length 17 #:alphabet-size 5)
    (many-random #:amount 1000 #:length 30 #:alphabet-size 26)
    (many-random #:amount 1000 #:length 100 #:alphabet-size 5)
    (many-uncompressable #:amount 10 #:length 100)

    (list
      #"abcd")


    (list
      (make-bytes 76 65)
      (make-bytes 200 65)
      (make-bytes 32769 65)
      #"\xAA\xBB"
      #"AAAAABAAAA"
      #"EEDACCBCCDCDDAC"
      #"aababcbacdbcacbcdaababcbaaccddbbccaaccbbd"
      #"aababcbabccdacdbcacbcdaabbbbbbaacdbcbaaccddbbccaaccbbd"

      (bytes-append
        (make-bytes 10000 65)
        (first (many-uncompressable #:amount 1 #:length 10000))))

    ))

(for ([input test-inputs])
  (define compressed (deflate-bytes input))
  (define decompressed (inflate-bytes* compressed))
  (unless (equal? input decompressed)
    (error 'inflate "Input and Decompressed output don't match: ~s ~s" input decompressed)))

(define large-input (first (many-random #:amount 1 #:length (expt 2 20) #:alphabet-size 26)))
(define large-compressed
  (deflate-bytes large-input))

(define N 5)

(for ([i N])
  (time (inflate-bytes large-compressed)))

(for ([i N])
  (time (inflate-bytes* large-compressed)))

;(require profile)
;(profile-thunk
;  #:delay 0.001
;  (lambda ()
;    (for ([i N])
;      (time (inflate-bytes* large-compressed)))))



