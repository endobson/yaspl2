#lang racket/base

(require
  "bootstrap-compiler.rkt"
  racket/runtime-path)

(define-runtime-path main-stub-list-file "../libraries/main-stub.src.list")
(run-bootstrap-compiler main-stub-list-file #"main_stub")
