#!/usr/bin/env racket
#lang racket/base

(require
  "compiler.rkt"
  "libraries.rkt"
  racket/file
  racket/runtime-path)

(define-runtime-path main-stub-list-file "../libraries/main-stub_lib.src.list")
(define-runtime-path compiler-list-file "../libraries/compiler_lib.src.list")
(define main-stub-files (file->bytes-lines main-stub-list-file))
(define compiler-files (file->bytes-lines compiler-list-file))

(define modules (load-modules (map bytes->path compiler-files)))

(define output-file
  (string->bytes/utf-8
    (vector-ref (current-command-line-arguments) 0)))


(let ([result (run-program modules 'compiler 'main #:stdin #""
                           #:args (list* output-file #"main_stub" main-stub-files))])
  (write-bytes (program-result-stdout result) (current-output-port))
  (write-bytes (program-result-stderr result) (current-error-port))
  (when (program-result-error-info result)
    (write-bytes (program-result-error-info result) (current-error-port))
    (newline (current-error-port)))
  (exit (program-result-exit-code result)))
