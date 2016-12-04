#!/usr/bin/env racket
#lang racket/base

(require
  "compiler.rkt"
  "libraries.rkt"
  racket/file
  racket/runtime-path)

(define-runtime-path library-list-file "../libraries/compiler-main.src.list")
(define-runtime-path src-root "..")
(define library-files
  (for/list ([file (in-list (file->bytes-lines library-list-file))])
    (build-path src-root (bytes->path file))))

(define modules (load-modules library-files))

(define output-file
  (string->bytes/utf-8
    (vector-ref (current-command-line-arguments) 0)))


(let ([result (run-program modules 'compiler-main 'main #:stdin #""
                           #:args (list* output-file #"compiler_main" (map path->bytes library-files)))])
  (write-bytes (program-result-stdout result) (current-output-port))
  (write-bytes (program-result-stderr result) (current-error-port))
  (when (program-result-error-info result)
    (write-bytes (program-result-error-info result) (current-error-port))
    (newline (current-error-port)))
  (exit (program-result-exit-code result)))
