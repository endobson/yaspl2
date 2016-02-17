#!/usr/bin/env racket
#lang racket/base

(require
  "interpreter.rkt"
  "libraries.rkt"
  racket/port
  racket/runtime-path)


(define library-files
  (list
    #"libraries/either.yaspl"
    #"libraries/maybe.yaspl"
    #"libraries/list.yaspl"
    #"libraries/bytes.yaspl"
    #"libraries/io.yaspl"
    #"libraries/numbers.yaspl"
    #"libraries/lexer.yaspl"
    #"libraries/sexp-parser.yaspl"
    #"libraries/arithmetic-expr.yaspl"
    #"libraries/tuples.yaspl"
    #"libraries/join-list.yaspl"
    #"libraries/dict.yaspl"
    #"libraries/stack-machine.yaspl"
    #"libraries/prim-implementation.yaspl"
    #"libraries/x86-64-stack-machine.yaspl"
    #"libraries/source-language.yaspl"
    #"libraries/source-to-stack.yaspl"
    #"libraries/compiler.yaspl"))


(define output-file
  (string->bytes/utf-8
    (vector-ref (current-command-line-arguments) 0)))


(let ([result (run-program modules 'compiler 'main #:stdin #""
                           #:args (list* output-file #"compiler" library-files))])
  (write-bytes (program-result-stdout result) (current-output-port))
  (write-bytes (program-result-stderr result) (current-error-port))
  (when (program-result-error-info result) 
    (write-bytes (program-result-error-info result) (current-error-port))
    (newline (current-error-port)))
  (exit (program-result-exit-code result)))
