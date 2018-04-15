#lang racket/base

(require
  "compiler.rkt"
  "libraries.rkt"
  "parser-structs.rkt"
  racket/file
  racket/runtime-path)

(provide
  run-bootstrap-compiler)

(define-runtime-path compiler-list-file "../libraries/compiler-main.src.list")
(define-runtime-path src-root "..")

(define (run-bootstrap-compiler file-list-file main-function)

  (define source-files
    (for/list ([file (in-list (file->bytes-lines file-list-file))])
      (path->bytes (build-path src-root (bytes->path file)))))
  (define compiler-files
    (for/list ([file (in-list (file->bytes-lines compiler-list-file))])
      (build-path src-root (bytes->path file))))

  (define-values (modules signatures) (load-modules compiler-files))

  (define output-file
    (string->bytes/utf-8
      (vector-ref (current-command-line-arguments) 0)))


  (let ([result (run-program modules signatures (module-name& '(compiler-main)) 'main #:stdin #""
                             #:args (list* output-file main-function source-files))])
    (write-bytes (program-result-stdout result) (current-output-port))
    (write-bytes (program-result-stderr result) (current-error-port))
    (when (program-result-error-info result)
      (write-bytes (program-result-error-info result) (current-error-port))
      (newline (current-error-port)))
    (exit (program-result-exit-code result))))
