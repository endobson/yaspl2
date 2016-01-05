#!/usr/bin/env racket
#lang racket/base
(require
  "interpreter.rkt"
  (submod "interpreter.rkt" modules)
  racket/runtime-path
  racket/set
  rackunit
  racket/match
  rackunit/text-ui)




(define (yaspl-test main-name
                    #:module-name module-name
                    #:modules [extra-modules null]
                    #:exit-code [exit-code 0]
                    #:stdin [stdin #""]
                    #:error [error-info #f]
                    #:stdout [stdout #""]
                    #:stderr [stderr #""])
  (test-suite (format "~a/~a" module-name main-name)
    (let* ([full-modules (set-union (list->set extra-modules) modules)]
           [result (run-program full-modules module-name main-name #:stdin stdin)])
      (check-equal? (program-result-exit-code result) exit-code)
      (check-equal? (program-result-error-info result) error-info)
      (when stdout
        (check-equal? (program-result-stdout result) stdout))
      (check-equal? (program-result-stderr result) stderr))))




(define (read-all port)
  (port-count-lines! port)
  (let loop ([l null])
    (let ([v (read port)])
      (if (eof-object? v)
          (reverse l)
          (loop (cons v l))))))

(define (kw-split kws)
  (define kw-hash (apply hash kws))
  (for/lists (kws vals) ([kw (in-list (sort (hash-keys kw-hash) keyword<?))])
    (values kw (hash-ref kw-hash kw))))

(define-runtime-path test-dir "tests")


(void (run-tests
  (test-suite "Yaspl tests"

    (make-test-suite ""
      (for/list ([file (in-directory test-dir)])
        (define-values (dir name-path is-dir) (split-path file))
        (when (symbol? name-path)
          (error 'tests "Bad path"))
        (define name (path->string name-path))
        (match (call-with-input-file* file read-all)
          [`(,new-modules ... #:test-cases ,test-cases ...)
           (make-test-suite ""
              (for/list ([tc (in-list test-cases)])
                (define-values (kws vals) (kw-split tc))
                (keyword-apply yaspl-test kws vals (list 'main)
                               #:modules (map parse-module new-modules))))])))


    (yaspl-test #:module-name 'lexer 'main #:stdin #"((((" #:exit-code 0)
    (yaspl-test #:module-name 'lexer 'main #:stdin #"()()()" #:exit-code 0)
    (yaspl-test #:module-name 'lexer 'main #:stdin #"(()" #:exit-code 0)
    (yaspl-test #:module-name 'lexer 'main #:stdin #"aaaa" #:exit-code 1)


    (yaspl-test #:module-name 'sexp-parser 'main #:stdin #"" #:exit-code 255 #:error #"Sexp result error")
    (yaspl-test #:module-name 'sexp-parser 'main #:stdin #"(" #:exit-code 255 #:error #"Sexp result error")
    (yaspl-test #:module-name 'sexp-parser 'main #:stdin #")" #:exit-code 255 #:error #"Sexp result error")
    (yaspl-test #:module-name 'sexp-parser 'main #:stdin #"()" #:exit-code 0)
    (yaspl-test #:module-name 'sexp-parser 'main #:stdin #"(()" #:exit-code 255 #:error #"Sexp result error")
    (yaspl-test #:module-name 'sexp-parser 'main #:stdin #"(()())" #:exit-code 0)
    (yaspl-test #:module-name 'sexp-parser 'main #:stdin #"( ( ()(( )  )\n )( ))" #:exit-code 0)
    (yaspl-test #:module-name 'sexp-parser 'main #:stdin #"+" #:exit-code 0)
    (yaspl-test #:module-name 'sexp-parser 'main #:stdin #"(+ (+))" #:exit-code 0)
    (yaspl-test #:module-name 'sexp-parser 'main #:stdin #"2" #:exit-code 0)
    (yaspl-test #:module-name 'sexp-parser 'main #:stdin #"23" #:exit-code 0)
    (yaspl-test #:module-name 'sexp-parser 'main #:stdin #"456" #:exit-code 0)
    (yaspl-test #:module-name 'sexp-parser 'main #:stdin #"(+ 2 3)" #:exit-code 0)

    (yaspl-test #:module-name 'arithmetic-expr 'main #:stdin #"2" #:exit-code 0)
    (yaspl-test #:module-name 'arithmetic-expr 'main #:stdin #"(+ 2 3)" #:exit-code 0)
    (yaspl-test #:module-name 'arithmetic-expr 'main #:stdin #"(+ (* 1 2) (- 4 3))" #:exit-code 0)

    (yaspl-test #:module-name 'stack-machine 'main #:stdin #"1" #:exit-code 0 #:stdout #"1\n")
    (yaspl-test #:module-name 'stack-machine 'main #:stdin #"(+ 1 2)" #:exit-code 0 #:stdout #"1\n2\n+\n")

    (yaspl-test #:module-name 'x86-64-stack-machine 'main #:stdin #"1" #:exit-code 0 #:stdout #f)
    (yaspl-test #:module-name 'x86-64-stack-machine 'main #:stdin #"(+ 1 2)" #:exit-code 0 #:stdout #f)
  )
  'verbose))
