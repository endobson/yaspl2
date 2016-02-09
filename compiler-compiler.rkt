#!/usr/bin/env racket
#lang racket/base

(require
  "interpreter.rkt"
  "libraries.rkt"
  racket/port
  racket/runtime-path)


(define-runtime-path library-dir "libraries")

(define library-files
  (list "either.yaspl" "maybe.yaspl" "list.yaspl" "bytes.yaspl" "io.yaspl" "numbers.yaspl"
        "lexer.yaspl" "sexp-parser.yaspl" "arithmetic-expr.yaspl" "tuples.yaspl"
        "join-list.yaspl" "dict.yaspl" "stack-machine.yaspl" "x86-64-stack-machine.yaspl"
        "source-language.yaspl" "source-to-stack.yaspl" "compiler.yaspl" "main.yaspl"))



(define stdin (apply bytes-append libraries))
(let ([result (run-program modules 'compiler 'main #:stdin stdin)])
  (write-bytes (program-result-stdout result) (current-output-port))
  (write-bytes (program-result-stderr result) (current-error-port))
  (when (program-result-error-info result) 
    (write-bytes (program-result-error-info result) (current-error-port))
    (newline (current-error-port)))
  (exit (program-result-exit-code result)))
