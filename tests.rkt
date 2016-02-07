#!/usr/bin/env racket
#lang racket/base
(require
  "interpreter.rkt"
  "libraries.rkt"
  "parser.rkt"
  racket/system
  racket/cmdline
  racket/list
  racket/file
  racket/promise
  racket/runtime-path
  racket/set
  racket/port
  rackunit
  racket/match
  rackunit/text-ui
  (for-syntax
    racket/base
    syntax/parse))



(define (yaspl-test #:module-name module-name
                    #:modules [extra-modules null]
                    #:exit-code [exit-code 0]
                    #:stdin [stdin #""]
                    #:error [error-info #f]
                    #:stdout [stdout #""]
                    #:stderr [stderr #""])
  (define full-modules (set-union (list->set extra-modules) modules))
  (test-case* (format "~a" module-name)
    (define result (run-program full-modules module-name 'main #:stdin stdin))
    (with-check-info
      (['exit-code (program-result-exit-code result)]
       ['error-info (program-result-error-info result)]
       ['stdout (program-result-stdout result)]
       ['stderr (program-result-stderr result)])
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
(define-runtime-path library-dir "libraries")

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
    (λ () (when (file-exists? file)
            (delete-file file)))))

(define-syntax test-case*
  (syntax-parser 
    [(_ name:expr bodies:expr ...)
     #'(parameterize ([current-test-case-around (lambda (t) (make-test-case name t))])
         (test-begin bodies ...))]))


(define (check-subprocess* #:error-message error-msg . args)
  (let ([stdout (open-output-bytes 'as-stdout)]
        [stderr (open-output-bytes 'as-stdout)]
        [stdin (open-input-bytes #"")])
    (match-define (list _ _ _ _ proc)
      (apply process*/ports stdout stdin stderr args))
    (proc 'wait)

    (unless (= 0 (proc 'exit-code))
     (with-check-info
        (['stdout (get-output-bytes stdout)]
         ['stderr (get-output-bytes stderr)])
       (fail-check error-msg)))))


(define (compiler-test program
                       #:mod compiler-mod
                       #:exit-code [exit-code 0]
                       #:stdout [stdout #""]
                       #:stdin [stdin #""])
  (test-case* "compiler test"
    (let ([result (run-program modules compiler-mod 'main #:stdin program)])
      (check-equal? (program-result-error-info result) #f)
      (check-equal? (program-result-exit-code result) 0)
      (call-with-temporary-files 3
        (λ (asm object binary)
          (call-with-output-file asm #:exists 'truncate
              (λ (p) (write-bytes (program-result-stdout result) p)))
          (check-subprocess* #:error-message "Assembler failed."
            "/usr/bin/env" "as" asm "-o" object)
          (check-subprocess* #:error-message "Linker failed."
            "/usr/bin/env" "ld" "-arch" "x86_64" "-macosx_version_min" "10.11"
            "-e" "_start" "-static" object "-o" binary)

          (define input (open-input-bytes stdin))
          (define output-port (open-output-bytes))
          (define error-output-port (open-output-bytes))
  
          (check-equal?
            (parameterize ([current-input-port input]
                           [current-output-port output-port]
                           [current-error-port error-output-port])
              (system*/exit-code binary))
            exit-code)
          (check-equal? (get-output-bytes output-port) stdout)
          (check-equal? (get-output-bytes error-output-port) #""))))))



(define compile-libraries-suite
  (let ()
    (define libraries
      (for/hash ([file (in-directory library-dir)])
        (define-values (dir name-path is-dir) (split-path file))
        (when (symbol? name-path)
          (error 'tests "Bad path"))
        (values
          (path->string name-path)
          (call-with-input-file* file port->bytes))))
    (make-test-suite "compile libraries"
      (for/list ([name (in-hash-keys libraries)])
        (make-test-suite name
          (list
            (let ()
              (define deps
                (case name
                  [("list.yaspl") (list "maybe.yaspl")]
                  [("bytes.yaspl") (list "maybe.yaspl" "list.yaspl")]
                  [("dict.yaspl") (list "maybe.yaspl" "list.yaspl" "tuples.yaspl")]
                  [("join-list.yaspl") (list "maybe.yaspl" "list.yaspl")]
                  [("io.yaspl") (list "maybe.yaspl" "list.yaspl" "bytes.yaspl")]
                  [("lexer.yaspl") (list "maybe.yaspl" "list.yaspl" "bytes.yaspl" "io.yaspl" "numbers.yaspl")]
                  [("sexp-parser.yaspl")
                   (list "either.yaspl" "maybe.yaspl" "list.yaspl" "bytes.yaspl" "io.yaspl" "numbers.yaspl"
                         "lexer.yaspl")]
                  [("arithmetic-expr.yaspl")
                   (list "either.yaspl" "maybe.yaspl" "list.yaspl" "bytes.yaspl" "io.yaspl" "numbers.yaspl"
                         "lexer.yaspl" "sexp-parser.yaspl")]
                  #;
                  [("source-language.yaspl")
                   (list "either.yaspl" "maybe.yaspl" "list.yaspl" "bytes.yaspl" "io.yaspl" "numbers.yaspl"
                         "lexer.yaspl" "sexp-parser.yaspl")]
                  [("stack-machine.yaspl")
                   (list "either.yaspl" "maybe.yaspl" "list.yaspl" "bytes.yaspl" "io.yaspl" "numbers.yaspl"
                         "lexer.yaspl" "sexp-parser.yaspl" "arithmetic-expr.yaspl" "tuples.yaspl"
                         "join-list.yaspl")]
                  #;
                  [("source-to-stack.yaspl")
                   (list "either.yaspl" "maybe.yaspl" "list.yaspl" "bytes.yaspl" "io.yaspl" "numbers.yaspl"
                         "lexer.yaspl" "sexp-parser.yaspl" "arithmetic-expr.yaspl" "tuples.yaspl"
                         "join-list.yaspl" "dict.yaspl" "stack-machine.yaspl" "source-language.yaspl")]
                  [("x86-64-stack-machine.yaspl")
                   (list "either.yaspl" "maybe.yaspl" "list.yaspl" "bytes.yaspl" "io.yaspl" "numbers.yaspl"
                         "lexer.yaspl" "sexp-parser.yaspl" "arithmetic-expr.yaspl" "tuples.yaspl"
                         "join-list.yaspl" "stack-machine.yaspl")]
                  #;
                  [("compiler.yaspl")
                   (list "either.yaspl" "maybe.yaspl" "list.yaspl" "bytes.yaspl" "io.yaspl" "numbers.yaspl"
                         "lexer.yaspl" "sexp-parser.yaspl" "arithmetic-expr.yaspl" "tuples.yaspl"
                         "join-list.yaspl" "dict.yaspl" "stack-machine.yaspl" "x86-64-stack-machine.yaspl"
                         "source-language.yaspl")]
                  [else empty]))
              (define module-name
                (case name
                  [("source-language.yaspl" "source-to-stack.yaspl" "compiler.yaspl")
                   'source-language]
                  [else 'compiler]))
              (define all-files (append deps (list name)))
              (define full-contents (apply bytes-append (map (λ (k) (hash-ref libraries k)) all-files)))

              (yaspl-test #:module-name module-name #:stdin full-contents #:stdout #f))))))))

(define run-test-files-suite
  (make-test-suite "test directory"
    (for/list ([file (in-directory test-dir)])
      (define-values (dir name-path is-dir) (split-path file))
      (when (symbol? name-path)
        (error 'tests "Bad path"))
      (define name (path->string name-path))
      (match (call-with-input-file* file read-all)
        [`(,new-modules ... #:test-cases ,test-cases ...)
         (make-test-suite name
            (for/list ([tc (in-list test-cases)] [i (in-naturals)])
              (define-values (kws vals) (kw-split tc))
              (make-test-suite (format "~a" i)
                (list
                  (keyword-apply yaspl-test kws vals null
                                 #:modules (map parse-module new-modules))))))]))))



(define run-all-tests #f)
(define run-with-timing #f)
(command-line
  #:once-each
    ("--full" "Whether to run the full test suite or not" (set! run-all-tests #t))
    ("--timing" "Whether to run with timing information or not" (set! run-with-timing #t)))

(define all-tests
  (make-test-suite "Yaspl tests"
    (list
      run-test-files-suite

      (yaspl-test #:module-name 'lexer #:stdin #"((((" #:exit-code 0)
      (yaspl-test #:module-name 'lexer #:stdin #"()()()" #:exit-code 0)
      (yaspl-test #:module-name 'lexer #:stdin #"(()" #:exit-code 0)
      (yaspl-test #:module-name 'lexer #:stdin #"aaaa" #:exit-code 0)
      (yaspl-test #:module-name 'lexer #:stdin #"#" #:exit-code 1)
      (yaspl-test #:module-name 'lexer #:stdin #"#:foo" #:exit-code 0)
      (yaspl-test #:module-name 'lexer #:stdin #"#\"foo" #:exit-code 1)
      (yaspl-test #:module-name 'lexer #:stdin #"#\"foo\"" #:exit-code 0)
      (yaspl-test #:module-name 'lexer #:stdin #"a;.&\na" #:exit-code 0)


      (yaspl-test #:module-name 'sexp-parser #:stdin #"" #:exit-code 255 #:error #"End of input")
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
      (yaspl-test #:module-name 'sexp-parser #:stdin #"#:foo" #:exit-code 0)
      (yaspl-test #:module-name 'sexp-parser #:stdin #"#\"foo\"" #:exit-code 0)
      (yaspl-test #:module-name 'sexp-parser #:stdin #";.&\na" #:exit-code 0)
      (yaspl-test #:module-name 'sexp-parser #:stdin #"(a;.&\na)" #:exit-code 0)
      (yaspl-test #:module-name 'sexp-parser #:stdin #"#t" #:exit-code 0)
      (yaspl-test #:module-name 'sexp-parser #:stdin #"(#t)" #:exit-code 0)


      (yaspl-test #:module-name 'sexp-parser #:stdin #"[]" #:exit-code 0)
      (yaspl-test #:module-name 'sexp-parser #:stdin #"[[]()]" #:exit-code 0)
      (yaspl-test #:module-name 'sexp-parser #:stdin #"[)" #:exit-code 255 #:error #"Sexp result error")
      (yaspl-test #:module-name 'sexp-parser #:stdin #"(]" #:exit-code 255 #:error #"Sexp result error")

      (yaspl-test #:module-name 'arithmetic-expr #:stdin #"(module (define (f) 2))" #:exit-code 0)
      (yaspl-test #:module-name 'arithmetic-expr #:stdin #"(module (define (f x) (+ 2 3)))" #:exit-code 0)
      (yaspl-test #:module-name 'arithmetic-expr
                  #:stdin #"(module (define (f x y) 2) (define (g z) (+ (* 1 2) (- 4 3))))" #:exit-code 0)
      (yaspl-test #:module-name 'arithmetic-expr
                  #:stdin #"(module (define (f) 2) (define (g) (f)))" #:exit-code 0)

      (yaspl-test #:module-name 'arithmetic-expr
                  #:stdin #"(module (define (f) (let ([x 3]) 2)))" #:exit-code 0)
      (yaspl-test #:module-name 'arithmetic-expr
                  #:stdin #"(module (define (f) (let ([x 3]) x)))" #:exit-code 0)

      (yaspl-test #:module-name 'x86-64-stack-machine
                  #:stdin #"(module (define (main) 1))" #:exit-code 0 #:stdout #f)
      (yaspl-test #:module-name 'x86-64-stack-machine
                  #:stdin #"(module (define (main) (+ 1 2)))" #:exit-code 0 #:stdout #f)


      (compiler-test #:mod 'x86-64-stack-machine
        #"(module (define (main_main) 0))")
      (compiler-test #:mod 'x86-64-stack-machine
        #"(module (define (main_main) (+ 1 1)))"
        #:exit-code 2)
      (compiler-test #:mod 'x86-64-stack-machine
        #"(module (define (main_main) (* (- 117 113) (+ 10 2))))"
        #:exit-code 48)
      (compiler-test #:mod 'x86-64-stack-machine
        #"(module (define (main_main) 1) (define (foo) 2))"
        #:exit-code 1)
      (compiler-test #:mod 'x86-64-stack-machine
        #"(module (define (main_main) (foo)) (define (foo) 2))"
        #:exit-code 2)
      (compiler-test #:mod 'x86-64-stack-machine
        #"(module (define (main_main) (let ([x 1]) 0)))")
      (compiler-test #:mod 'x86-64-stack-machine
        #"(module (define (main_main) (let ([x 1]) x)))"
        #:exit-code 1)
      (compiler-test #:mod 'x86-64-stack-machine
        #"(module (define (main_main) (let ([x 1]) (+ x (let ([y 2]) (* y x))))))"
        #:exit-code 3)
      (compiler-test #:mod 'x86-64-stack-machine
        #"(module
            (define (main_main) (f 2))
            (define (f x) (+ x (g 3 4)))
            (define (g y z) (* y z)))"
        #:exit-code 14)


      (compiler-test #:mod 'compiler
        #"(module main (import) (export) (types)
            (define (main) : Byte 0))"
        #:exit-code 0)
      (compiler-test #:mod 'compiler
        #"(module main (import) (export) (types)
            (define (main) : Byte (let ([x 5]) x)))"
        #:exit-code 5)
      (compiler-test #:mod 'compiler
        #"(module main (import) (export) (types)
            (define (main) : Byte (f 4))
            (define (f [x : Byte]) : Byte x))"
        #:exit-code 4)
      (compiler-test #:mod 'compiler
        #"(module main (import (prim +)) (export) (types)
            (define (main) : Byte (+ 4 5)))"
        #:exit-code 9)
      (compiler-test #:mod 'compiler
        #"(module main (import) (export) (types)
            (define (main) : Byte (let ([x #t]) 1)))"
        #:exit-code 1)
      (compiler-test #:mod 'compiler
        #"(module main (import) (export) (types)
            (define (main) : Byte (if #t 1 0)))"
        #:exit-code 1)
      (compiler-test #:mod 'compiler
        #"(module main (import (prim bytes-ref)) (export) (types)
            (define (main) : Byte (let ([x #\"abc\"]) (bytes-ref x 1))))"
        #:exit-code 98)
      (compiler-test #:mod 'compiler
        #"(module main (import (prim bytes-ref make-bytes)) (export) (types)
            (define (main) : Byte (let ([x (make-bytes 4 16)]) (bytes-ref x 1))))"
        #:exit-code 16)
      (compiler-test #:mod 'compiler
        #"(module main (import (prim bytes-ref make-bytes bytes-set!)) (export) (types)
            (define (main) : Byte
              (let ([x (make-bytes 4 16)])
                (begin
                  (bytes-set! x 2 3)
                  (bytes-ref x 2)))))"
        #:exit-code 3)
      (compiler-test #:mod 'compiler
        #"(module main (import (prim + bytes-ref make-bytes bytes-set!)) (export) (types)
            (define (main) : Byte
              (let ([x (make-bytes 4 16)])
                (begin
                  (bytes-set! x 2 0)
                  (+
                    (bytes-ref x 1)
                    (bytes-ref x 3))))))"
        #:exit-code 32)
      (compiler-test #:mod 'compiler
        #"(module main (import (prim bytes-length make-bytes)) (export) (types)
            (define (main) : Byte (let ([x (make-bytes 4 0)]) (bytes-length x))))"
        #:exit-code 4)
      (compiler-test #:mod 'compiler
        #"(module main (import (prim write-bytes)) (export) (types)
            (define (main) : Byte (write-bytes #\"abc\" 1 0 3)))"
        #:stdout #"abc"
        #:exit-code 3)
      (compiler-test #:mod 'compiler
        #"(module main
            (import)
            (export)
            (types
              (define-type Foo
                (foo [v Byte])))
            (define (main) : Byte (let ([f (foo 4)]) 0)))"
        #:exit-code 0)
      (compiler-test #:mod 'compiler
        #"(module main
            (import)
            (export)
            (types
              (define-type Foo
                (foo [v Byte])))
            (define (main) : Byte
              (foo-v (foo 7))))"
        #:exit-code 7)
      (compiler-test #:mod 'compiler
        #"(module main
            (import (prim *))
            (export)
            (types
              (define-type Foo
                (foo [x Byte] [y Byte] [z Byte])))
            (define (main) : Byte
              (let ([f1 (foo 1 2 3)])
                (let ([f2 (foo 4 5 6)])
                  (* (foo-y f2) (foo-z f1))))))"
        #:exit-code 15)
      (compiler-test #:mod 'compiler
        #"(module main
            (import)
            (export)
            (types
              (define-type Foo
                (foo [x Byte] [y Byte] [z Byte])))
            (define (main) : Byte
              (case (foo 1 2 3)
                [_ 5])))"
        #:exit-code 5)
      (compiler-test #:mod 'compiler
        #"(module main
            (import)
            (export)
            (types
              (define-type Foo
                (foo [x Byte] [y Byte] [z Byte])))
            (define (main) : Byte
              (case (foo 1 2 3)
                [x (foo-y x)])))"
        #:exit-code 2)
      (compiler-test #:mod 'compiler
        #"(module main
            (import)
            (export)
            (types
              (define-type Foo
                (foo)
                (bar)))
            (define (main) : Byte
              (let ([a (foo)])
                (let ([b (bar)])
                  (case a
                    [(foo)
                     (case b
                       [(foo) 1]
                       [(bar) 2])]
                    [(bar)
                     (case b
                       [(foo) 3]
                       [(bar) 4])])))))"
        #:exit-code 2)
      (compiler-test #:mod 'compiler
        #"(module main
            (import)
            (export)
            (types
              (define-type Tree
                (branch [left Tree] [right Tree])
                (leaf)))
            (define (main) : Byte
              (case (branch (leaf) (leaf))
                [(branch (branch _ _) _) 1]
                [(branch _ (branch _ _)) 2]
                [(branch _ _) 3])))"
        #:exit-code 3)

      (compiler-test #:mod 'compiler
        #"(module main
            (import (prim * - +))
            (export)
            (types
              (define-type Tree
                (branch [left Tree] [right Tree])
                (leaf [v Byte])))
            (define (main) : Byte
              (+
                (* 100 (foo (branch (leaf 1) (branch (leaf 1) (leaf 0)))))
                (+ (* 10 (foo (branch (branch (leaf 1) (leaf 2)) (leaf 2))))
                   (foo (branch (leaf 4) (leaf 3))))))
            (define (foo [x : Tree]) : Byte
              (case x
                [(branch (branch (leaf a) (leaf b)) (leaf c)) (* a (* b c))]
                [(branch (leaf a) (branch (leaf b) (leaf c))) (+ a (+ b c))]
                [(branch (leaf a) (leaf b)) (- a b)])))"
        #:exit-code 241)


      (compiler-test #:mod 'compiler
        #"(module other
            (import)
            (export foo)
            (types)
            (define (foo) : Byte 5))
          (module main
            (import (other foo))
            (export)
            (types)
            (define (main) : Byte (foo)))"
        #:exit-code 5)


      (compiler-test #:mod 'compiler
        #"(module main
            (import (prim =))
            (export)
            (types)
            (define (main) : Byte (if (= 5 5) 2 3)))"
        #:exit-code 2)
      (compiler-test #:mod 'compiler
        #"(module main
            (import (prim <))
            (export)
            (types)
            (define (main) : Byte (if (< 3 5) 2 3)))"
        #:exit-code 2)
      (compiler-test #:mod 'compiler
        #"(module main
            (import (prim >))
            (export main)
            (types)
            (define (main) : Byte (if (> 3 5) 2 3)))"
        #:exit-code 3)
      (compiler-test #:mod 'compiler
        #"(module main
            (import (prim <=))
            (export)
            (types)
            (define (main) : Byte (if (<= 3 5) 2 3)))"
        #:exit-code 2)
      (compiler-test #:mod 'compiler
        #"(module main
            (import (prim >=))
            (export)
            (types)
            (define (main) : Byte (if (>= 3 5) 2 3)))"
        #:exit-code 3)

      (compiler-test #:mod 'compiler
        #"(module main
            (import (prim void))
            (export)
            (types)
            (define (main) : Byte (begin (void) 0)))"
        #:exit-code 0)

      (compiler-test #:mod 'compiler
        #"(module main (import (prim read-bytes make-bytes)) (export) (types)
            (define (main) : Byte
              (let ([buf (make-bytes 4 0)])
                (read-bytes buf 0 0 3))))"
        #:stdin #"abc"
        #:exit-code 3)


      (if run-all-tests
          compile-libraries-suite
          (make-test-suite "No compile-libraries" empty)))))

(break-enabled #f)
(define results
  (delay/thread
    (if run-with-timing
        (foldts-test-suite
          (λ (suite name before after acc) (cons name acc))
          (λ (suite name before after acc child-acc) acc)
          (λ (case name action acc)
             (write (reverse (cons name acc)))
             (newline)
             (define result (time (run-test-case name action)))
             (display-result result)
             (display-context result)
             acc)
          empty
          all-tests)
        (run-tests all-tests 'verbose))))

(with-handlers ([exn:break? (λ (e) (exit 1))])
  (void (sync/enable-break results)))
