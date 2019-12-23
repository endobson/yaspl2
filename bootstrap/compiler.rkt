#lang racket/base

(require
  "machine-structs.rkt"
  "primitives.rkt"
  "racketize/module.rkt"
  "topo-sort.rkt"
  racket/list
  racket/set
  racket/hash)
(provide
  run-program
  (struct-out program-result))


(struct program-result (exit-code error-info stdout stderr))
(define-namespace-anchor anchor)

(define (run-program modules signatures module-name main-name #:stdin stdin-bytes
                     #:args [supplied-args empty])
  (define racket-modules (racketize-modules modules signatures))
  (define ns (namespace-anchor->empty-namespace anchor))
  (parameterize ([current-namespace ns])
    (namespace-require 'racket/base))
  (define main-fun
    (eval-syntax
      #`(begin
          (module prim racket/base
            (provide (all-defined-out))
            #,@(for/list ([(prim-name prim-val) (in-hash supported-primitives)])
                `(define ,prim-name ,prim-val)))
          #,@racket-modules
          (require
            (only-in '#,(mod-name->racket-mod-name module-name)
              [#,main-name main-fun]))
          main-fun)
      ns))

  (define process-args (cons #"/binary-path" supplied-args))
  (define merged-process-args
    (apply bytes-append (map (lambda (arg) (bytes-append arg #"\0")) process-args)))
  (define stdin (open-input-bytes stdin-bytes 'stderr))
  (define stdout (open-output-bytes 'stdout))
  (define stderr (open-output-bytes 'stderr))

  (define return-val
    (let/ec exit-k
      (parameterize ([exit-parameter exit-k])
        (main-fun merged-process-args stdin stdout stderr))))

  (program-result
    (if (error-sentinal? return-val) 255 return-val)
    (and (error-sentinal? return-val) (error-sentinal-info return-val))
    (get-output-bytes stdout)
    (get-output-bytes stderr)))


(define (racketize-modules modules signatures)
  (for/list ([module (topo-sort (set->list modules))])
    (racketize-module module signatures)))
