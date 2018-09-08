#lang racket/base

(require
  "bootstrap-compiler.rkt"
  racket/runtime-path)

(define-runtime-path library-compiler-list-file "../libraries/library-compiler.src.list")
(run-bootstrap-compiler library-compiler-list-file #"library_compiler_main")
