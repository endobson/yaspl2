#!/usr/bin/env racket
#lang racket/base

(require
  "compiler.rkt"
  "libraries.rkt"
  racket/file
  racket/runtime-path)

(define-runtime-path library-compiler-list-file "../libraries/library-compiler-main.src.list")
(define-runtime-path compiler-list-file "../libraries/compiler_lib.src.list")
(define-runtime-path src-root "..")
(define library-compiler-files
  (for/list ([file (in-list (file->bytes-lines library-compiler-list-file))])
    (path->bytes (build-path src-root (bytes->path file)))))
(define compiler-files
  (for/list ([file (in-list (file->bytes-lines compiler-list-file))])
    (build-path src-root (bytes->path file))))

(define modules (load-modules compiler-files))

(define output-file
  (string->bytes/utf-8
    (vector-ref (current-command-line-arguments) 0)))


(let ([result (run-program modules 'compiler 'main #:stdin #""
                           #:args (list* output-file #"library_compiler_main" library-compiler-files))])
  (write-bytes (program-result-stdout result) (current-output-port))
  (write-bytes (program-result-stderr result) (current-error-port))
  (when (program-result-error-info result)
    (write-bytes (program-result-error-info result) (current-error-port))
    (newline (current-error-port)))
  (exit (program-result-exit-code result)))
