#lang racket/base

(require
  "machine-structs.rkt"
  "parser-structs.rkt"
  "primitives.rkt"
  "utils.rkt"
  racket/list
  racket/set
  racket/match)
(provide
  run-program
  module&-name
  topo-sort
  (struct-out program-result))



(define (topo-sort modules)
  (define module-hash
    (for/hash ([module (in-list modules)])
      (values (module&-name module) module)))

  (define edges
    (hash-copy
      (for/hash ([module (in-list modules)])
        (define imports
          (list->mutable-set
            (append
              (map import&-module-name (imports&-types (module&-imports module)))
              (map import&-module-name (imports&-values (module&-imports module))))))
        ;; Remove primitive module until we support module signatures
        (set-remove! imports 'prim)
        (values (module&-name module) imports))))

  (define reverse-edges (make-hash))
  (for* ([(src dests) (in-hash edges)]
         [dest (in-set dests)])
    (set-add! (hash-ref! reverse-edges dest (λ () (mutable-set))) src))

  (define empty-nodes (mutable-set))
  (for ([(src dests) (in-hash edges)]
        #:when (set-empty? dests))
    (set-add! empty-nodes src))
  (for ([mod (in-set empty-nodes)])
    (hash-remove! edges empty-nodes))
  (define order empty)

  (let loop ()
    (unless (set-empty? empty-nodes)
      (define mod (set-first empty-nodes))
      (set-remove! empty-nodes mod)
      (for ([mod2 (in-set (hash-ref reverse-edges mod (set)))])
        (define links (hash-ref edges mod2))
        (set-remove! links mod)
        (when (set-empty? links)
          (set-add! empty-nodes mod2)
          (hash-remove! edges mod2)))
      (set! order (cons mod order))
      (loop)))
  (unless (= (length order) (length modules))
    (error 'topo-sort "Something went wrong: ~n~a~n~a" order (map module&-name modules)))
  (map (λ (name) (hash-ref module-hash name)) (reverse order)))





;; Ties the knot of recursive global functions
(define (make-global-env modules)
  (define (make-primitive-environment)
    (hash-copy
      (for/hash ([prim (in-list supported-primitives)])
        (values (full-name 'prim prim) (prim-function-val prim)))))


  (define global-env (make-primitive-environment))


  (for ([module (topo-sort (set->list modules))])
    (define local-env (make-hash))

    (for ([import (in-list (imports&-values (module&-imports module)))])
      (hash-set! local-env (import&-name import)
                 (hash-ref global-env
                   (full-name (import&-module-name import) (import&-name import)))))

    (for ([type (in-list (module&-types module))])
      (for ([variant (in-list (define-type&-variants type))])
        (define variant-name (variant&-name variant))
        (hash-set! local-env variant-name
          (variant-constructor-val
            variant-name
            (map variant-field&-name (variant&-fields variant))))

        (for ([field (variant&-fields variant)] [index (in-naturals)])
          (define field-name (variant-field&-name field))
          (hash-set! local-env
            (string->symbol (format "~a-~a" variant-name field-name))
            (field-accessor-val variant-name index)))))

    (for ([(name def) (in-hash (module&-definitions module))])
      (define val
        (match def
          [(definition& _ args body)
           (function-val args local-env body)]))
      (hash-set! local-env name val))
    (for ([export (in-list (module&-exports module))])
      (define name (export&-name export))
      (define local-val (hash-ref local-env name #f))
      (when local-val
        (hash-set! global-env (full-name (module&-name module) name) local-val))))
  global-env)

(struct program-result (exit-code error-info stdout stderr))

(define (run-program modules module-name main-name #:stdin stdin-bytes)
  (define env (make-global-env modules))
  (define full-main-name (full-name module-name main-name))
  (define main-fun (hash-ref env full-main-name #f))
  (unless main-fun
    (error 'run-program "Main function is not exported: ~s in ~s" full-main-name module-name))
  (unless (function-val? main-fun)
    (error 'run-program "Main function is not a function value: ~s" main-fun))
  (unless (equal? (length (function-val-args main-fun)) 3)
    (error 'run-program "Main function does not have correct arity: ~s" main-fun))

  (define stdout (open-output-bytes 'stdout))
  (define stderr (open-output-bytes 'stderr))
  (define stdin (open-input-bytes stdin-bytes 'stderr))
  (define args (list (prim-port-val stdin) (prim-port-val stdout) (prim-port-val stderr)))

  (define return-val (call-function main-fun args (halt-k)))
  (program-result
    (if (error-sentinal? return-val) 255 (byte-val-v return-val))
    (and (error-sentinal? return-val) (error-sentinal-info return-val))
    (get-output-bytes stdout)
    (get-output-bytes stderr)))




(define (call-function fun args cont)
  (match fun
    [(function-val arg-names env body)
     (if (= (length args) (length arg-names))
         (let ([new-env
                 (for/fold ([env (hash-copy/immutable env)])
                           ([v (in-list args)] [name (in-list arg-names)])
                   (hash-set env name v))])
           (run-eval body new-env cont))
         (error-sentinal
           (string->bytes/utf-8
             (format "Wrong number of arguments: Expected ~a, got ~a"
                     (length arg-names) (length args)))))]
    [(variant-constructor-val variant-name fields)
     (unless (= (length args) (length fields))
       (error 'variant-constructor "Wrong number of arguments for ~a: Expected ~a, got ~a"
              variant-name (length fields) (length args)))
     (run-cont (variant-val variant-name args) cont)]
    [(field-accessor-val variant-name index)
     (match args
       [(list (variant-val (== variant-name equal?) fields))
        (run-cont (list-ref fields index) cont)]
       [_
         (error-sentinal #"Wrong variant")])]
    [(prim-function-val name)
     (match (run-primitive name args)
       [(? value? val) (run-cont val cont)]
       [(error-sentinal info) (error-sentinal info)])]))


(define (run-eval expr env cont)
  (match expr
    [(byte& v)
     (run-cont (byte-val v) cont)]
    [(bytes& v)
     (run-cont (bytes-val v) cont)]
    [(boolean& v)
     (run-cont (boolean-val v) cont)]
    [(variable& v)
     (define val (hash-ref env v #f))
     (if val
         (run-cont val cont)
         (error-sentinal (string->bytes/utf-8 (format "No binding for ~a available" v))))]
    [(app& op vs)
     (run-apply empty (cons op vs) env cont)]
    [(if& cond true false)
     (run-eval cond env (if-k true false env cont))]
    [(begin& first-expr exprs)
     (run-eval first-expr env
       (for/fold ([cont cont]) ([expr (in-list (reverse exprs))])
         (ignore-k expr env cont)))]
    [(case& expr clauses)
     (run-eval expr env (case-k clauses env cont))]
    [(let& name expr body)
     (run-eval expr env (bind-k name body env cont))]))

(define (run-apply vals exprs env cont)
  (if (empty? exprs)
      (let ([vals (reverse vals)])
        (call-function (first vals) (rest vals) cont))
      (run-eval
        (first exprs)
        env
        (apply-k vals (rest exprs) env cont))))

(define (run-cont val cont)
  (match cont
    [(halt-k) val]
    [(ignore-k expr env cont)
     (run-eval expr env cont)]
    [(apply-k vals args env cont)
     (run-apply (cons val vals) args env cont)]
    [(if-k true false env cont)
     (define expr (if (boolean-val-v val) true false))
     (run-eval expr env cont)]
    [(case-k clauses env cont)
     (match val
       [(variant-val variant-name _)
        (define clause
          (for/first ([clause (in-list clauses)]
                      #:when (equal?
                               (abstraction-pattern&-name (case-clause&-pattern clause))
                               variant-name))
            clause))
        (unless clause
          (error 'case "No match for ~a in ~a"
                 variant-name (map case-clause&-pattern clauses)))
        (define new-env
          (for/fold ([env env]) ([arg-name (in-list (map variable-pattern&-v
                                                         (abstraction-pattern&-patterns
                                                           (case-clause&-pattern clause))))]
                                 [field-val (in-list (variant-val-fields val))])
            (hash-set env arg-name field-val)))

        (run-eval (case-clause&-expr clause) new-env cont)])]
    [(bind-k name body env cont)
     (run-eval body (hash-set env name val) cont)]))

