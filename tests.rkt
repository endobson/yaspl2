#!/usr/bin/env racket
#lang racket/base
(require
  "interpreter.rkt"
  (submod "interpreter.rkt" modules)
  rackunit
  rackunit/text-ui)




(define (yaspl-test module-name main-name
                    #:exit-code [exit-code 0]
                    #:stdin [stdin #""]
                    #:error [error-info #f]
                    #:stdout [stdout #""]
                    #:stderr [stderr #""])
  (test-suite (format "~a/~a" module-name main-name)
    (let ([result (run-program modules module-name main-name #:stdin stdin)])
      (check-equal? (program-result-exit-code result) exit-code)
      (check-equal? (program-result-error-info result) error-info)
      (when stdout
        (check-equal? (program-result-stdout result) stdout))
      (check-equal? (program-result-stderr result) stderr))))




(void (run-tests
  (test-suite "Yaspl tests"
    (yaspl-test 'exit-code 'main #:exit-code 1)
    (yaspl-test 'exit-code2 'main #:exit-code 2)
    (yaspl-test 'exit-code3 'main #:exit-code 3)
    (yaspl-test 'exit-code4 'main #:exit-code 4)
    (yaspl-test 'exit-code5 'main #:exit-code 5)
    (yaspl-test 'exit-code6 'main #:exit-code 6)

    (yaspl-test 'stdout1 'main #:stdout #"Aa")
    (yaspl-test 'stdin1 'main #:stdin #"A" #:exit-code 65)
    (yaspl-test 'stdin2 'main #:stdin #"123" #:exit-code 123)
    (yaspl-test 'panic1 'main #:exit-code 255 #:error #"\0\0\0")
    (yaspl-test 'panic2 'main #:exit-code 255 #:error #"Boom")
    (yaspl-test 'echo1 'main #:stdin #"Hello world" #:stdout #"Hello world")
    (yaspl-test 'echo2 'main #:stdin #"Hello world" #:stdout #"Hello world")
    (yaspl-test 'sum-tree 'main #:exit-code 15)


    (yaspl-test 'lexer 'main #:stdin #"((((" #:exit-code 0)
    (yaspl-test 'lexer 'main #:stdin #"()()()" #:exit-code 0)
    (yaspl-test 'lexer 'main #:stdin #"(()" #:exit-code 0)
    (yaspl-test 'lexer 'main #:stdin #"aaaa" #:exit-code 1)


    (yaspl-test 'sexp-parser 'main #:stdin #"" #:exit-code 255 #:error #"Sexp result error")
    (yaspl-test 'sexp-parser 'main #:stdin #"(" #:exit-code 255 #:error #"Sexp result error")
    (yaspl-test 'sexp-parser 'main #:stdin #")" #:exit-code 255 #:error #"Sexp result error")
    (yaspl-test 'sexp-parser 'main #:stdin #"()" #:exit-code 0)
    (yaspl-test 'sexp-parser 'main #:stdin #"(()" #:exit-code 255 #:error #"Sexp result error")
    (yaspl-test 'sexp-parser 'main #:stdin #"(()())" #:exit-code 0)
    (yaspl-test 'sexp-parser 'main #:stdin #"( ( ()(( )  )\n )( ))" #:exit-code 0)
    (yaspl-test 'sexp-parser 'main #:stdin #"+" #:exit-code 0)
    (yaspl-test 'sexp-parser 'main #:stdin #"(+ (+))" #:exit-code 0)
    (yaspl-test 'sexp-parser 'main #:stdin #"2" #:exit-code 0)
    (yaspl-test 'sexp-parser 'main #:stdin #"23" #:exit-code 0)
    (yaspl-test 'sexp-parser 'main #:stdin #"456" #:exit-code 0)
    (yaspl-test 'sexp-parser 'main #:stdin #"(+ 2 3)" #:exit-code 0)

    (yaspl-test 'arithmetic-expr 'main #:stdin #"2" #:exit-code 0)
    (yaspl-test 'arithmetic-expr 'main #:stdin #"(+ 2 3)" #:exit-code 0)
    (yaspl-test 'arithmetic-expr 'main #:stdin #"(+ (* 1 2) (- 4 3))" #:exit-code 0)

    (yaspl-test 'stack-machine 'main #:stdin #"1" #:exit-code 0 #:stdout #"1\n")
    (yaspl-test 'stack-machine 'main #:stdin #"(+ 1 2)" #:exit-code 0 #:stdout #"1\n2\n+\n")

    (yaspl-test 'x86-64-stack-machine 'main #:stdin #"1" #:exit-code 0 #:stdout #f)
    (yaspl-test 'x86-64-stack-machine 'main #:stdin #"(+ 1 2)" #:exit-code 0 #:stdout #f)
  )
  'verbose))
