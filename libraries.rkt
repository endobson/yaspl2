#lang racket/base

(require
  "interpreter.rkt"
  racket/runtime-path
  racket/set)
(provide modules)



(define modules (mutable-set))
(define module-names (mutable-set))

(define (read-module port)
  (port-count-lines! port)
  (begin0
    (read port)
    (let ([remaining (read port)])
      (unless (eof-object? remaining)
        (error 'read-module "Extra stuff after module: ~a" remaining)))))

(define (add-module! mod-src)
  (define mod (parse-module mod-src))
  (check-module mod)
  (construct-module-signature mod (hash))
  (define mod-name (module&-name mod))
  (when (set-member? module-names mod-name)
    (error 'add-module! "Cannot add module: ~s is already defined" mod-name))
  (set-add! module-names mod-name)
  (set-add! modules mod))

(define-runtime-path libraries-dir "libraries")
(for ([file (in-directory libraries-dir)])
  (define-values (dir name-path is-dir) (split-path file))
  (when (symbol? name-path)
    (error 'tests "Bad path"))
  (add-module! (call-with-input-file* file read-module)))
