#lang racket/base

(require
  "machine-structs.rkt"
  "parser-structs.rkt"
  "primitives.rkt"
  "utils.rkt"
  "topo-sort.rkt"
  "simple-match.rkt"
  racket/list
  racket/syntax
  racket/set
  syntax/stx
  racket/hash
  racket/match)
(provide
  run-program
  module&-name
  topo-sort
  (struct-out program-result))


(struct program-result (exit-code error-info stdout stderr))
(define-namespace-anchor anchor)

(define (run-program modules module-name main-name #:stdin stdin-bytes
                     #:args [supplied-args empty])
  (define-values (definitions env) (compile-modules modules))
  (define full-main-name (full-name module-name main-name))
  (define main-fun-id (hash-ref env full-main-name #f))
  (unless main-fun-id
    (error 'run-program "Main function is not exported: ~s in ~s" full-main-name module-name))

  (define ns (namespace-anchor->empty-namespace anchor))
  (parameterize ([current-namespace ns])
    (namespace-require 'racket/base))
  (define main-fun
    (eval
      `(let ()
          ,@definitions
          ,main-fun-id)
      ns))

  (define process-args (array-val (list->vector (map bytes-val (cons #"/binary-path" supplied-args)))))
  (define stdin (open-input-bytes stdin-bytes 'stderr))
  (define stdout (open-output-bytes 'stdout))
  (define stderr (open-output-bytes 'stderr))

  (define return-val
    (let/ec exit-k
      (parameterize ([exit-parameter exit-k])
        (main-fun process-args (prim-port-val stdin) (prim-port-val stdout) (prim-port-val stderr)))))

  (program-result
    (if (error-sentinal? return-val) 255 (byte-val-v return-val))
    (and (error-sentinal? return-val) (error-sentinal-info return-val))
    (get-output-bytes stdout)
    (get-output-bytes stderr)))

(define define-sym (datum->syntax #'define 'define))
(define app-sym (datum->syntax #'#%app '#%app))
(define lambda-sym (datum->syntax #'lambda 'lambda))
(define list-ref-sym (datum->syntax #'list-ref 'list-ref))
(define variant-val-sym (datum->syntax #'variant-val 'variant-val))
(define variant-val-fields-sym (datum->syntax #'variant-val-fields 'variant-val-fields))

(define (compile-modules modules)
  (define (make-primitive-environment)
    (hash-copy
      (for/hash ([(prim-name prim-val) (in-hash supported-compiled-primitives)])
        (values (full-name 'prim prim-name) prim-val))))

  ;; Mapping to identifiers
  (define global-env (make-primitive-environment))

  (define definitions
    (for/list ([module (topo-sort (set->list modules))])
      ;;mapping to identifiers
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



      (define variant-defs
        (for/list ([type (in-list (module&-types module))])
          (for/list ([variant (in-list (define-type&-variants type))])
            (define variant-name (variant&-name variant))
            (define constructor-id (generate-temporary variant-name))
            (hash-set! local-env variant-name constructor-id)
            (hash-set! local-pattern-env variant-name variant-name)

            (cons
              `(,define-sym (,constructor-id . vs) (,app-sym ,variant-val-sym ',variant-name vs))
              (for/list ([field (variant&-fields variant)] [index (in-naturals)])
                (define field-name (variant-field&-name field))
                (define field-id (generate-temporary field-name))
                (hash-set! local-env
                  (string->symbol (format "~a-~a" variant-name field-name))
                  field-id)
                `(,define-sym (,field-id v) (,app-sym ,list-ref-sym (,app-sym ,variant-val-fields-sym v) ',index)))))))


      (for ([(name _) (in-hash (module&-definitions module))])
        (define temporary (generate-temporary name))
        (hash-set! local-env name temporary))

      (define immutable-local-env (hash-copy/immutable local-env))
      (define immutable-local-pattern-env (hash-copy/immutable local-pattern-env))

      (define function-defs
        (for/list ([(name def) (in-hash (module&-definitions module))])
          (match def
            [(definition& _ args body)
             (define temporaries (generate-temporaries args))
             `(,define-sym (,(hash-ref local-env name) ,@temporaries)
                 ,(compile-expr
                      immutable-local-pattern-env
                      (for/fold ([env immutable-local-env])
                                ([a (in-list args)] [t (in-list temporaries)])
                        (hash-set env a t))
                      body))])))

      (for ([export (in-list (module&-exports module))])
        (match-define (export& in-name out-name) export)
        (define local-val (hash-ref local-env in-name #f))
        (when local-val
          (hash-set! global-env (full-name (module&-name module) out-name) local-val)))
      (append (append* (append* variant-defs)) function-defs)))
  (values
    (append* definitions)
    global-env))


;; env is hash table to expressions which evaluate to the value
(define (compile-expr pat-env env expr)
  (match expr
    [(byte& v)
     `(,#'byte-val ',v)]
    [(bytes& v)
     `(,#'bytes-val ',v)]
    [(boolean& v)
     `(,#'boolean-val ',v)]
    [(variable& v)
     (hash-ref env v (lambda () (error 'compile-expr "Unbound variables ~a" v)))]
    [(app& op args)
     (match-define (cons op-id arg-ids) (generate-temporaries (cons op args)))

     (define-values (bindings ids)
       (for/lists (bindings ids)
                  ([v (in-list (cons op args))]
                   [v-id (in-list (cons op-id arg-ids))])
         (define compiled-expr (compile-expr pat-env env v))
         (if (identifier? compiled-expr)
             (values empty compiled-expr)
             (values
               (list `[(,v-id) ,compiled-expr])
               v-id))))
     (define flattened-bindings (append* bindings))

     (if (empty? flattened-bindings)
         `(,#'#%app ,@ids)
         `(,#'let-values (,@flattened-bindings) (,#'#%app ,@ids)))]
    [(varargs-app& op args)
     (match-define (cons op-id arg-ids) (generate-temporaries (cons op args)))

     (define-values (bindings ids)
       (for/lists (bindings ids)
                  ([v (in-list (cons op args))]
                   [v-id (in-list (cons op-id arg-ids))])
         (define compiled-expr (compile-expr pat-env env v))
         (if (identifier? compiled-expr)
             (values empty compiled-expr)
             (values
               (list `[,v-id ,compiled-expr])
               v-id))))
     (define flattened-bindings (append* bindings))

     (if (empty? flattened-bindings)
         `(,@ids)
         `(,#'let (,@flattened-bindings)
            (,#'#%app ,(first ids) (,#'#%app ,#'array-val (,#'#%app ,#'vector ,@(rest ids))))))]

    [(if& cond true false)
     `(,#'if ,`(,#'boolean-val-v ,(compile-expr pat-env env cond))
           ,(compile-expr pat-env env true)
           ,(compile-expr pat-env env false))]
    [(begin& first-expr exprs)
     `(,#'begin ,@(map (λ (e) (compile-expr pat-env env e)) (cons first-expr exprs)))]
    [(let& name expr body)
     (define compiled-expr (compile-expr pat-env env expr))
     (cond
       [(identifier? compiled-expr)
        (compile-expr pat-env (hash-set env name compiled-expr) body)]
       [else
        (define temp (generate-temporary name))
        `(,#'let ([,temp ,compiled-expr])
            ,(compile-expr pat-env (hash-set env name temp) body))])]
    [(lambda& (list (list arg-names _) ...) body)
     (define ids (generate-temporaries arg-names))
     (define new-env
       (for/fold ([env env]) ([name (in-list arg-names)] [id (in-list ids)])
         (hash-set env name id)))

     `(,#'lambda (,@ids) ,(compile-expr pat-env new-env body))]
    [(case& expr clauses)
     (define form
       (for/fold ([form #'(error 'end-of-case)]) ([clause (in-list (reverse clauses))])
         (match-define (case-clause& pattern expr) clause)
         (let-values ([(triple-function vars env) (compile-pattern/simple-match pattern pat-env env)])
           (define body (compile-expr pat-env env expr))
            #`(#,app-sym #,triple-function
                 val
                 (#,lambda-sym (#,@vars) #,body)
                 (#,lambda-sym () #,form)))))
       #`(let ([val #,(compile-expr pat-env env expr)]) #,form)]
    #;
    [(case& expr clauses)
     (define match-clauses
       (for/list ([clause (in-list clauses)])
         (match-define (case-clause& pattern expr) clause)
         (let-values ([(match-pat env) (compile-pattern pattern pat-env env)])
           (define body (compile-expr pat-env env expr))
           `[,match-pat ,body])))
     `(,#'match ,(compile-expr pat-env env expr) ,@match-clauses)]))

;; Pattern (Hash Symbol Symbol) (Hash Symbol Indentifier) -> (Values Syntax (Hash Symbol Identifier))
(define (compile-pattern p pat-env env)
  (define (recur p env)
    (match p
      [(bytes-pattern& bytes)
       (values `(,#'bytes-val ,bytes) env)]
      [(byte-pattern& byte)
       (values `(,#'byte-val ,byte) env)]
      [(variable-pattern& var)
       (define id (generate-temporary var))
       (values id (hash-set env var id))]
      [(ignore-pattern&)
       (values #'_ env)]
      [(abstraction-pattern& pattern-binding pats)
       (let-values
         ([(vs env)
           (for/fold ([vs empty] [env env]) ([pat (in-list (reverse pats))])
             (let-values ([(v env) (recur pat env)])
                (values (cons v vs) env)))])
         (values `(,#'variant-val ',(hash-ref pat-env pattern-binding) (list ,@vs)) env))]))
  (recur p env))
