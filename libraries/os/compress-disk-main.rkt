#lang racket/base

(require
  file/gzip
  racket/match
  racket/port)

(match-define
  (vector input-path output-path)
  (current-command-line-arguments))

(time
  (call-with-input-file input-path
    (lambda (input-port)
      (call-with-output-file output-path
        (lambda (output-port)
          (gzip-through-ports input-port output-port #f 0))))))

