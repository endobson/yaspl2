#lang racket/base

(require
  "machine-structs.rkt"
  "parser-structs.rkt"
  "primitives.rkt"
  "utils.rkt"
  "topo-sort.rkt"
  racket/list
  racket/set
  racket/hash
  racket/match)
(provide
  run-program
  module&-name
  topo-sort
  (struct-out program-result))


(struct environment (values patterns))


;; Ties the knot of recursive global functions
(define (make-global-env modules)
  (define (make-primitive-environment)
    (hash-copy
      (for/hash ([(prim-name prim-val) (in-hash supported-primitives)])
        (values (full-name 'prim prim-name) prim-val))))


  (define global-env (make-primitive-environment))


  (for ([module (topo-sort (set->list modules))])
    (define local-env (make-hash))
    (define local-pattern-env (make-hash))

    (for ([import (in-list (imports&-values (module&-imports module)))])
      (hash-set! local-env (import&-local-name import)
                 (hash-ref global-env
                   (full-name (import&-module-name import) (import&-exported-name import)))))

    ;; TODO actually support more complicated pattern bindings
    (for ([import (in-list (imports&-patterns (module&-imports module)))])
      (hash-set! local-pattern-env (import&-local-name import)
                 (import&-exported-name import)))


    (for ([type (in-list (module&-types module))])
      (for ([variant (in-list (define-type&-variants type))])
        (define variant-name (variant&-name variant))
        (hash-set! local-env variant-name
          (variant-constructor-val
            variant-name
            (map variant-field&-name (variant&-fields variant))))
        (hash-set! local-pattern-env variant-name variant-name)

        (for ([field (variant&-fields variant)] [index (in-naturals)])
          (define field-name (variant-field&-name field))
          (hash-set! local-env
            (string->symbol (format "~a-~a" variant-name field-name))
            (field-accessor-val variant-name index)))))

    (define env-box (box #f))
    (for ([(name def) (in-hash (module&-definitions module))])
      (define val
        (match def
          [(definition& _ args body)
           (function-val args env-box body)]))
      (hash-set! local-env name val))
    (set-box! env-box (environment (hash-copy/immutable local-env)
                                   (hash-copy/immutable local-pattern-env)))

    (for ([export (in-list (exports&-values (module&-exports module)))])
      (match-define (export& in-name out-name) export)
      (define local-val (hash-ref local-env in-name #f))
      (when local-val
        (hash-set! global-env (full-name (module&-name module) out-name) local-val))))
  global-env)

(struct program-result (exit-code error-info stdout stderr))

(define (run-program modules module-name main-name #:stdin stdin-bytes #:args [supplied-args empty])
  (define env (make-global-env modules))
  (define full-main-name (full-name module-name main-name))
  (define main-fun (hash-ref env full-main-name #f))
  (unless main-fun
    (error 'run-program "Main function is not exported: ~s in ~s" full-main-name module-name))
  (unless (function-val? main-fun)
    (error 'run-program "Main function is not a function value: ~s" main-fun))
  (unless (equal? (length (function-val-args main-fun)) 4)
    (error 'run-program "Main function does not have correct arity: ~s" main-fun))

  (define stdout (open-output-bytes 'stdout))
  (define stderr (open-output-bytes 'stderr))
  (define stdin (open-input-bytes stdin-bytes 'stderr))
  (define process-args (array-val (list->vector (map bytes-val (cons #"/binary-path" supplied-args)))))
  (define args (list process-args (prim-port-val stdin) (prim-port-val stdout) (prim-port-val stderr)))

  (define return-val (call-function main-fun args (halt-k)))
  (program-result
    (if (error-sentinal? return-val) 255 (byte-val-v return-val))
    (and (error-sentinal? return-val) (error-sentinal-info return-val))
    (get-output-bytes stdout)
    (get-output-bytes stderr)))




(define (call-function fun args cont)
  (match fun
    [(function-val arg-names env-box body)
     (if (= (length args) (length arg-names))
         (let ([new-env
                 (for/fold ([env (unbox env-box)])
                           ([v (in-list args)] [name (in-list arg-names)])
                   (variable-set env name v))])
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
       [(list (variant-val value-name _))
        (error-sentinal
          (string->bytes/utf-8
            (format "Wrong variant: Expected ~a, got ~a" variant-name value-name)))])]
    [(compiled-function-val f)
     (f args (λ (v) (run-cont v cont)))]))


(define (run-eval expr env cont)
  (match expr
    [(byte& v)
     (run-cont (byte-val v) cont)]
    [(bytes& v)
     (run-cont (bytes-val v) cont)]
    [(boolean& v)
     (run-cont (boolean-val v) cont)]
    [(variable& v)
     (define val (variable-ref env v #f))
     (if val
         (run-cont val cont)
         (error-sentinal (string->bytes/utf-8 (format "No binding for ~a available" v))))]
    [(app& op vs)
     (run-apply empty (cons op vs) env cont)]
    [(varargs-app& op vs)
     (run-varargs-apply empty (cons op vs) env cont)]
    [(if& cond true false)
     (run-eval cond env (if-k true false env cont))]
    [(begin& first-expr exprs)
     (run-eval first-expr env
       (for/fold ([cont cont]) ([expr (in-list (reverse exprs))])
         (ignore-k expr env cont)))]
    [(case& expr clauses)
     (run-eval expr env (case-k clauses env cont))]
    [(lambda& (list (list args _) ...) body)
     (run-cont (function-val args (box-immutable env) body) cont)]
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

(define (run-varargs-apply vals exprs env cont)
  (if (empty? exprs)
      (let ([vals (reverse vals)])
        (call-function (first vals) (list (array-val (list->vector (rest vals)))) cont))
      (run-eval
        (first exprs)
        env
        (varargs-apply-k vals (rest exprs) env cont))))


(define (run-cont val cont)
  (match cont
    [(halt-k) val]
    [(ignore-k expr env cont)
     (run-eval expr env cont)]
    [(apply-k vals args env cont)
     (run-apply (cons val vals) args env cont)]
    [(varargs-apply-k vals args env cont)
     (run-varargs-apply (cons val vals) args env cont)]
    [(if-k true false env cont)
     (define expr (if (boolean-val-v val) true false))
     (run-eval expr env cont)]
    [(case-k clauses env cont)
     (define match-result
       (for*/first ([clause (in-list clauses)]
                    [res (in-value (match-pattern (case-clause&-pattern clause) val env))]
                    #:when res)
         (list (case-clause&-expr clause) res)))
     (unless match-result
       (error 'case "No match for ~a in ~a"
              val (map case-clause&-pattern clauses)))
     (match-define (list expr value-map) match-result)
     (run-eval expr value-map cont)]
    [(bind-k name body env cont)
     (run-eval body (variable-set env name val) cont)]))

;; Returns either #f or Environment
(define (match-pattern p v env)
  (define (recur pvs acc)
    (match pvs
      [(list) acc]
      [(cons (list (bytes-pattern& bytes) v) pvs)
       (match v
         [(bytes-val v-bytes)
          (and (equal? bytes v-bytes) (recur pvs acc))])]
      [(cons (list (byte-pattern& byte) v) pvs)
       (match v
         [(byte-val v-val)
          (and (equal? byte v-val) (recur pvs acc))])]
      [(cons (list (variable-pattern& var) v) pvs)
       (recur pvs (variable-set acc var v))]
      [(cons (list (ignore-pattern&) _) pvs)
       (recur pvs acc)]
      [(cons (list (abstraction-pattern& pattern-binding field-patterns) v) pvs)
       (define pattern-name (pattern-ref env pattern-binding))
       (match v
         [(variant-val name fields)
          (and (equal? name pattern-name)
               (recur (append (map list field-patterns fields) pvs) acc))])]))
  (recur (list (list p v)) env))


(define (pattern-ref env name)
  (match env
    [(environment vals pats)
     (hash-ref pats name)]))

(define (variable-set env name value)
  (match env
    [(environment vals pats)
     (environment (hash-set vals name value) pats)]))

(define (variable-ref env name default)
  (match env
    [(environment vals pats)
     (hash-ref vals name default)]))
