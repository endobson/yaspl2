#!/usr/bin/env racket
#lang racket/base

(require 
  racket/match
  racket/port
  racket/string
  racket/system)

(void (system "bazel build //libraries:wasm-compiler"))


(define (run-test name value)
  (define wasm-module
    (with-output-to-bytes
      (lambda ()
        (define exit-code
          (system*/exit-code
            "bazel-bin/libraries/wasm-compiler" 
            (format "(yaspl examples ~a)" name)
            (format "libraries/yaspl/examples/~a.yaspl" name)))
        (unless (zero? exit-code)
          (error 'run-test "Couldn't compile ~s; error-code ~s" name exit-code)))))


  (define test-output
    (call-with-output-string
      (lambda (out)
        (define in (open-input-bytes wasm-module))
        (define err (open-output-bytes))
        (match-define (list #f #f _ #f subproc)
          (process*/ports out in err (find-executable-path "wasmtime") "-" "--invoke" "main" "0"))
        (subproc 'wait)
        (unless (zero? (subproc 'exit-code))
          (error 'run-test "Couldn't run ~s:~n~a" name (get-output-bytes err))))))

  (define test-output-numeric (string->number (string-trim test-output)))

  (unless (equal? test-output-numeric value)
    (error 'run-test "Output for ~s is not as expected: ~s vs ~s" name value test-output-numeric)))


(run-test "const-int" 42)
(run-test "const-boolean" 1)
(run-test "if1" 10)
(run-test "if2" 20)
(run-test "function1" 100)
(run-test "function2" 200)
(run-test "function3" 100)
(run-test "math1" 4)
(run-test "not" 9)
(run-test "int-case" 400)
