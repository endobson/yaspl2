#lang racket/base

(require
  "compiler.rkt"
  "libraries.rkt"
  "parser-structs.rkt"
  racket/file
  racket/match
  racket/runtime-path
  racket/vector)

(provide
  run-bootstrap-compiler)

(define-runtime-path compiler-list-file "../libraries/compiler.src.list")
(define-runtime-path src-root "..")

(define (run-bootstrap-compiler file-list-file main-function)
  (define compiler-files
    (for/list ([file (in-list (file->bytes-lines compiler-list-file))])
      (build-path src-root (bytes->path file))))
  (define source-files
    (for/list ([file (in-list (file->bytes-lines file-list-file))])
      file))
  (define prim-files
    (list
      (path->bytes (build-path src-root "libraries/yaspl/runtime/fill-stack.prim"))
      (path->bytes (build-path src-root "libraries/yaspl/runtime/read-memory.prim"))))

  (define-values (modules signatures) (load-modules compiler-files))

  (match-define (vector output-format output-file)
    (vector-map
      string->bytes/utf-8
      (current-command-line-arguments)))


  (let ([result (run-program modules signatures (module-name& '(compiler-main)) 'main #:stdin #""
                             #:args `(,output-format ,output-file ,main-function ,@source-files ,@prim-files))])
    (write-bytes (program-result-stdout result) (current-output-port))
    (write-bytes (program-result-stderr result) (current-error-port))
    (when (program-result-error-info result)
      (define err (program-result-error-info result))
      ((error-display-handler) (exn-message err) err))
    (exit (program-result-exit-code result))))
