#lang racket/base

(require
  "bootstrap-compiler.rkt"
  racket/runtime-path)

(define-runtime-path compiler-list-file
  "../libraries/prim-language/prim-language-library-compiler.src.list")
(run-bootstrap-compiler compiler-list-file #"prim_language_library_compiler_main")
