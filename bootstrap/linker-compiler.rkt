#lang racket/base

(require
  "bootstrap-compiler.rkt"
  racket/runtime-path)

(define-runtime-path linker-list-file "../libraries/linker.src.list")
(run-bootstrap-compiler linker-list-file #"linker_main")
