#!/usr/bin/env racket
#lang racket/base
(require
  "interpreter.rkt"
  "libraries.rkt"
  racket/system
  racket/file
  racket/runtime-path
  racket/set
  rackunit
  racket/match
  rackunit/text-ui)




(define (yaspl-test #:module-name module-name
                    #:modules [extra-modules null]
                    #:exit-code [exit-code 0]
                    #:stdin [stdin #""]
                    #:error [error-info #f]
                    #:stdout [stdout #""]
                    #:stderr [stderr #""])
  (test-suite (format "~a" module-name)
    (let* ([full-modules (set-union (list->set extra-modules) modules)]
           [result (run-program full-modules module-name 'main #:stdin stdin)])
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

(define (call-with-temporary-files n f)
  (if (= n 0)
      (f)
      (call-with-temporary-file
        (λ (file)
           (call-with-temporary-files
             (sub1 n)
             (λ files (apply f (cons file files))))))))

(define (call-with-temporary-file f)
  (define file (make-temporary-file))
  (dynamic-wind
    void
    (λ () (f file))
    (λ () (delete-file file))))


(define (compiler-test program #:exit-code [exit-code 0])
  (test-suite ""
    (let ([result (run-program modules 'x86-64-stack-machine 'main #:stdin program)])
      (test-begin
        (check-equal? (program-result-error-info result) #F)
        (check-equal? (program-result-exit-code result) 0)
        (call-with-temporary-files 3
          (λ (asm object binary)
            (call-with-output-file asm #:exists 'truncate
                (λ (p) (write-bytes (program-result-stdout result) p)))
            (system* "/usr/bin/env" "as" asm "-o" object)
            (system* "/usr/bin/env" "ld" "-arch" "x86_64" "-macosx_version_min" "10.11"
                     "-e" "_start" "-static" object "-o" binary)
            (check-equal? (system*/exit-code binary) exit-code)))))))

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
                (keyword-apply yaspl-test kws vals null
                               #:modules (map parse-module new-modules))))])))


    (yaspl-test #:module-name 'lexer #:stdin #"((((" #:exit-code 0)
    (yaspl-test #:module-name 'lexer #:stdin #"()()()" #:exit-code 0)
    (yaspl-test #:module-name 'lexer #:stdin #"(()" #:exit-code 0)
    (yaspl-test #:module-name 'lexer #:stdin #"aaaa" #:exit-code 0)


    (yaspl-test #:module-name 'sexp-parser #:stdin #"" #:exit-code 255 #:error #"Sexp result error")
    (yaspl-test #:module-name 'sexp-parser #:stdin #"(" #:exit-code 255 #:error #"Sexp result error")
    (yaspl-test #:module-name 'sexp-parser #:stdin #")" #:exit-code 255 #:error #"Sexp result error")
    (yaspl-test #:module-name 'sexp-parser #:stdin #"()" #:exit-code 0)
    (yaspl-test #:module-name 'sexp-parser #:stdin #"(()" #:exit-code 255 #:error #"Sexp result error")
    (yaspl-test #:module-name 'sexp-parser #:stdin #"(()())" #:exit-code 0)
    (yaspl-test #:module-name 'sexp-parser #:stdin #"( ( ()(( )  )\n )( ))" #:exit-code 0)
    (yaspl-test #:module-name 'sexp-parser #:stdin #"+" #:exit-code 0)
    (yaspl-test #:module-name 'sexp-parser #:stdin #"(+ (+))" #:exit-code 0)
    (yaspl-test #:module-name 'sexp-parser #:stdin #"2" #:exit-code 0)
    (yaspl-test #:module-name 'sexp-parser #:stdin #"23" #:exit-code 0)
    (yaspl-test #:module-name 'sexp-parser #:stdin #"456" #:exit-code 0)
    (yaspl-test #:module-name 'sexp-parser #:stdin #"(+ 2 3)" #:exit-code 0)

    (yaspl-test #:module-name 'arithmetic-expr #:stdin #"(module (define (f) 2))" #:exit-code 0)
    (yaspl-test #:module-name 'arithmetic-expr #:stdin #"(module (define (f x) (+ 2 3)))" #:exit-code 0)
    (yaspl-test #:module-name 'arithmetic-expr
                #:stdin #"(module (define (f x y) 2) (define (g z) (+ (* 1 2) (- 4 3))))" #:exit-code 0)
    (yaspl-test #:module-name 'arithmetic-expr
                #:stdin #"(module (define (f) 2) (define (g) (f)))" #:exit-code 0)

    (yaspl-test #:module-name 'x86-64-stack-machine
                #:stdin #"(module (define (main) 1))" #:exit-code 0 #:stdout #f)
    (yaspl-test #:module-name 'x86-64-stack-machine
                #:stdin #"(module (define (main) (+ 1 2)))" #:exit-code 0 #:stdout #f)

    (compiler-test #"(module (define (main) 0))")
    (compiler-test #"(module (define (main) (+ 1 1)))" #:exit-code 2)
    (compiler-test #"(module (define (main) (* (- 117 113) (+ 10 2))))" #:exit-code 48)
    (compiler-test #"(module (define (main) 1) (define (foo) 2))" #:exit-code 1)
    (compiler-test #"(module (define (main) (foo)) (define (foo) 2))" #:exit-code 2)


  )
  'verbose))
