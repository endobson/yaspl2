#lang racket/base

(require
  "bootstrap-compiler.rkt"
  racket/cmdline)

(define source-list* #f)
(define main* #f)
(command-line 
  #:once-each
  ("--source-list" source-list "Relative path to file with list of source files." 
   (set! source-list* source-list))
  ("--main" main "Module name containing main function." 
   (set! main* (string->bytes/utf-8 main)))
  #:args args (current-command-line-arguments (list->vector args)))

(run-bootstrap-compiler source-list* main*)
