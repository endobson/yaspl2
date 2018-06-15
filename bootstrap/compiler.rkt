#lang racket/base

(require
  "machine-structs.rkt"
  "parser-structs.rkt"
  "primitives.rkt"
  "utils.rkt"
  "topo-sort.rkt"
  "signature-structs.rkt"
  "simple-match.rkt"
  racket/list
  racket/syntax
  racket/set
  syntax/stx
  racket/hash
  racket/match)
(provide
  run-program
  (struct-out program-result))


(struct program-result (exit-code error-info stdout stderr))
(define-namespace-anchor anchor)

(define (run-program modules signatures module-name main-name #:stdin stdin-bytes
                     #:args [supplied-args empty])
  (define-values (definitions env) (compile-modules modules signatures))
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

  (define process-args (list->vector (cons #"/binary-path" supplied-args)))
  (define stdin (open-input-bytes stdin-bytes 'stderr))
  (define stdout (open-output-bytes 'stdout))
  (define stderr (open-output-bytes 'stderr))

  (define return-val
    (let/ec exit-k
      (parameterize ([exit-parameter exit-k])
        (main-fun process-args stdin stdout stderr))))

  (program-result
    (if (error-sentinal? return-val) 255 return-val)
    (and (error-sentinal? return-val) (error-sentinal-info return-val))
    (get-output-bytes stdout)
    (get-output-bytes stderr)))

(define define-sym (datum->syntax #'define 'define))
(define app-sym (datum->syntax #'#%plain-app '#%plain-app))
(define lambda-sym (datum->syntax #'lambda 'lambda))
(define let-sym (datum->syntax #'let 'let))
(define vector-sym (datum->syntax #'vector 'vector))
(define vector-ref-sym (datum->syntax #'vector-ref 'vector-ref))
(define variant-val-sym (datum->syntax #'variant-val 'variant-val))
(define variant-val-fields-sym (datum->syntax #'variant-val-fields 'variant-val-fields))

(define (compile-modules modules signatures)
  (define (make-primitive-environment)
    (hash-copy
      (for/hash ([(prim-name prim-val) (in-hash supported-primitives)])
        (values (full-name (module-name& '(prim)) prim-name) prim-val))))

  ;; Mapping to identifiers
  (define global-env (make-primitive-environment))

  (define definitionss
    (for/list ([module (topo-sort (set->list modules))])
      (define-values (definitions new-global-entries)
        (compile-module module signatures (hash-copy/immutable global-env)))
      (hash-union! global-env new-global-entries)
      definitions))
  (values
    (append* definitionss)
    global-env))

(define (compile-module module signatures global-env)
  ;;mapping to identifiers
  (define local-env (make-hash))
  (define local-pattern-env (make-hash))

  (for ([imports (in-list (module&-imports module))])
    (match imports
      [(partial-imports& mod-name _ values _)
       (for ([import (in-list values)])
         (hash-set! local-env (import&-local-name import)
                    (hash-ref global-env
                      (full-name mod-name (import&-exported-name import)))))]
      [(full-imports& mod-name)
       (match (hash-ref signatures mod-name)
         [(module-signature _ values _ _)
          (for ([export (in-hash-keys values)])
            (hash-set! local-env export
                       (hash-ref global-env
                         (full-name mod-name export))))])]))


  ;; TODO actually support more complicated pattern bindings
  (for ([imports (in-list (module&-imports module))])
    (match imports
      [(partial-imports& mod-name _ _ patterns)
       (for ([import (in-list patterns)])
         (hash-set! local-pattern-env (import&-local-name import)
                    (import&-exported-name import)))]
      [(full-imports& mod-name)
       (match (hash-ref signatures mod-name)
         [(module-signature _ _ _ patterns)
          (for ([export (in-hash-keys patterns)])
            (hash-set! local-pattern-env export export))])]))


  (define variant-defs
    (append*
      (append*
        (for/list ([type (in-list (module&-types module))])
          (for/list ([variant (in-list (define-type&-variants type))])
            (define variant-name (variant&-name variant))
            (define constructor-id (generate-temporary variant-name))
            (hash-set! local-env variant-name constructor-id)
            (hash-set! local-pattern-env variant-name variant-name)

            (cons
              (with-syntax ([vs (generate-temporaries (variant&-fields variant))])
                (if (zero? (length (syntax->list #'vs)))
                    (let ([v (generate-temporary constructor-id)])
                      `(,define-sym ,constructor-id
                         (,let-sym ([,v (,app-sym ,variant-val-sym ',variant-name (,app-sym ,vector-sym))])
                           (,lambda-sym () ,v))))
                    `(,define-sym (,constructor-id . ,#'vs)
                       (,app-sym ,variant-val-sym ',variant-name (,app-sym ,vector-sym . ,#'vs)))))
              (for/list ([field (variant&-fields variant)] [index (in-naturals)])
                (define field-name (variant-field&-name field))
                (define field-id (generate-temporary field-name))
                (hash-set! local-env
                  (string->symbol (format "~a-~a" variant-name field-name))
                  field-id)
                `(,define-sym (,field-id v)
                    (,app-sym ,vector-ref-sym (,app-sym ,variant-val-fields-sym v)
                              ',index)))))))))


  (for ([(name _) (in-hash (module&-definitions module))])
    (define temporary (generate-temporary name))
    (hash-set! local-env name temporary))

  (define immutable-local-env (hash-copy/immutable local-env))
  (define immutable-local-pattern-env (hash-copy/immutable local-pattern-env))

  (define function-defs
    (for/list ([(name def) (in-hash (module&-definitions module))])
      (match def
        [(definition& _ args (block& defs body))
         (define temporaries (generate-temporaries args))
         `(,define-sym (,(hash-ref local-env name) ,@temporaries)
             ,(compile-block
                  immutable-local-pattern-env
                  (for/fold ([env immutable-local-env])
                            ([a (in-list args)] [t (in-list temporaries)])
                    (hash-set env a t))
                  defs
                  body))])))

  (values
    (append variant-defs function-defs)
    (for/hash ([export (in-list (exports&-values (module&-exports module)))])
      (match-define (export& in-name out-name) export)
      (values
        (full-name (module&-name module) out-name)
        (hash-ref local-env in-name)))))

(define (compile-block pat-env env defs body)
  (match defs
    [(list)
     (compile-expr pat-env env body)]
    [(cons (match-def& pattern expr) defs)
     (define vars (pattern-variables pattern))
     (define body-vars (generate-temporaries vars))
     (define body-env
       (for/fold ([env env]) ([var (in-list vars)] [id (in-list body-vars)])
         (hash-set env var id)))
     #`(let ([val #,(compile-expr pat-env env expr)]
             [succ (#,lambda-sym (#,@body-vars) #,(compile-block pat-env body-env defs body))])
         (#,app-sym #,(compile-pattern pattern pat-env vars)
          val succ (#,lambda-sym () (error 'match))))]))

;; env is hash table to expressions which evaluate to the value
(define (compile-expr pat-env env expr)
  (match expr
    [(byte& v)
     `',v]
    [(bytes& v)
     `',v]
    [(boolean& v)
     `',v]
    [(variable& v)
     (hash-ref env v (lambda () (error 'compile-expr "Unbound variables ~a" v)))]
    [(app& op args)
     `(,#'#%app
       ,@(for/list ([v (in-list (cons op args))])
           (compile-expr pat-env env v)))]
    [(varargs-app& op args)
     `(,#'#%app
        ,(compile-expr pat-env env op)
        (,#'#%app ,#'vector
                  ,@(for/list ([arg (in-list args)])
                      (compile-expr pat-env env arg))))]

    [(if& cond true false)
     `(,#'if ,(compile-expr pat-env env cond)
           ,(compile-expr pat-env env true)
           ,(compile-expr pat-env env false))]
    [(begin& first-expr exprs)
     `(,#'begin ,@(map (λ (e) (compile-expr pat-env env e)) (cons first-expr exprs)))]
    [(ann& _ expr)
     (compile-expr pat-env env expr)]
    [(let& name expr body)
     (define compiled-expr (compile-expr pat-env env expr))
     (cond
       [(identifier? compiled-expr)
        (compile-expr pat-env (hash-set env name compiled-expr) body)]
       [else
        (define temp (generate-temporary name))
        `(,#'let ([,temp ,compiled-expr])
            ,(compile-expr pat-env (hash-set env name temp) body))])]
    [(lambda& (list (list arg-names _) ...) _ body)
     (define ids (generate-temporaries arg-names))
     (define new-env
       (for/fold ([env env]) ([name (in-list arg-names)] [id (in-list ids)])
         (hash-set env name id)))

     `(,#'lambda (,@ids) ,(compile-expr pat-env new-env body))]
    [(case& expr clauses)
     (define form
       (for/fold ([form #'(error 'end-of-case)]) ([clause (in-list (reverse clauses))])
         (match-define (case-clause& pattern (block& defs expr)) clause)
         (define vars (pattern-variables pattern))
         (define body-vars (generate-temporaries vars))
         (define body-env
           (for/fold ([env env]) ([var (in-list vars)] [id (in-list body-vars)])
             (hash-set env var id)))
         (define body (compile-block pat-env body-env defs expr))
         #`(#,app-sym #,(compile-pattern pattern pat-env vars)
            val
            (#,lambda-sym (#,@body-vars) #,body)
            (#,lambda-sym () #,form))))

     #`(let ([val #,(compile-expr pat-env env expr)]) #,form)]))

(define (pattern-variables p)
  (define (recur p acc)
    (match p
      [(bytes-pattern& bytes) acc]
      [(byte-pattern& byte) acc]
      [(variable-pattern& var)
       (cons var acc)]
      [(ignore-pattern&)
       acc]
      [(abstraction-pattern& pattern-binding pats)
       (for/fold ([acc acc]) ([pat (in-list pats)])
         (recur pat acc))]))
  (recur p empty))
