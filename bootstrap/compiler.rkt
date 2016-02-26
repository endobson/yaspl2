#!/usr/bin/env racket
#lang racket/base

(require
  "interpreter.rkt"
  "libraries.rkt"
  racket/port)
(define stdin (port->bytes (current-input-port)))
(let ([result (run-program modules 'compiler 'main #:stdin stdin)])
  (write-bytes (program-result-stdout result) (current-output-port))
  (write-bytes (program-result-stderr result) (current-error-port))
  (when (program-result-error-info result) 
    (write-bytes (program-result-error-info result) (current-error-port))
    (newline (current-error-port)))
  (exit (program-result-exit-code result)))
