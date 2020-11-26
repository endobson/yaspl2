#lang racket/base

(require
  (for-syntax
    racket/base
    syntax/parse))

(provide
  bytes-set!/u16-le
  bytes-set!/u32-le
  bytes-set!/u64-le
  make-section
  define-section
  write-all-bytes
  ascii->utf-16
  ascii-bytes->ucs-2)

(define (bytes-set!/u16-le bytes offset v)
  (integer->integer-bytes v 2 #f #f bytes offset))
(define (bytes-set!/u32-le bytes offset v)
  (integer->integer-bytes v 4 #f #f bytes offset))
(define (bytes-set!/u64-le bytes offset v)
  (integer->integer-bytes v 8 #f #f bytes offset))

(define-syntax (make-section stx)
  (syntax-parse stx
    [(_ name:id #:size size:expr bodies:expr ...)
     #'(let ([name (make-bytes size)])
         bodies ...
         (bytes->immutable-bytes name))]))

(define-syntax (define-section stx)
  (syntax-parse stx
    [(_ name:id #:size size:expr bodies:expr ...)
     #'(define name (make-section name #:size size bodies ...))]))

(define (write-all-bytes b p)
  (let loop ([offset 0])
    (when (< offset (bytes-length b))
      (define written (write-bytes b p offset))
      (loop (+ offset written)))))

(define (ascii->utf-16 str)
  (define b (make-bytes (* (string-length str) 2)))
  (for ([i (string-length str)])
    (define code-point (char->integer (string-ref str i)))
    (bytes-set! b (* 2 i) code-point))
  (bytes->immutable-bytes b))

(define (ascii-bytes->ucs-2 input)
  (define b (make-bytes (* (bytes-length input) 2)))
  (for ([i (bytes-length input)])
    (define code-point (bytes-ref input i))
    (bytes-set! b (* 2 i) code-point))
  (bytes->immutable-bytes b))
