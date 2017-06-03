#lang racket/base

(require
  "parser.rkt"
  "parser-structs.rkt"
  "topo-sort.rkt"
  "validator.rkt"
  "primitives.rkt"
  racket/runtime-path
  racket/set)
(provide load-modules)


(define (load-modules paths)
  (define modules (mutable-set))
  (define module-signatures (make-hash (list (cons (module-name& '(prim)) primitive-module-signature))))
  (define module-names (mutable-set))

  (define (read-module port)
    (port-count-lines! port)
    (let loop ([sexps null])
      (define sexp (read port))
      (if (eof-object? sexp)
          (reverse sexps)
          (loop (cons sexp sexps)))))

  (define (add-module!/internal mod)
    (define mod-name (module&-name mod))
    (check-module mod)
    (define mod-signature
      (construct-module-signature mod module-signatures))
    (when (set-member? module-names mod-name)
      (error 'add-module! "Cannot add module: ~s is already defined" mod-name))
    (set-add! module-names mod-name)
    (hash-set! module-signatures mod-name mod-signature)
    (set-add! modules mod))


  (define (add-modules! mod-srcs)
    (define mods (topo-sort (map parse-module mod-srcs)))
    (for-each add-module!/internal mods))


  (add-modules!
    (for/list ([file-path (in-list paths)])
      (call-with-input-file* file-path read-module)))
  (set->list modules))
