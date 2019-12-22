#lang racket/base

(require
  "machine-structs.rkt"
  "parser-structs.rkt"
  "primitives.rkt"
  "utils.rkt"
  "topo-sort.rkt"
  "signature-structs.rkt"
  "racketize/pattern.rkt"
  racket/list
  racket/syntax
  racket/set
  syntax/stx
  racket/hash
  racket/runtime-path
  racket/match)
(provide
  run-program
  (struct-out program-result))

(define-runtime-path machine-structs-path "machine-structs.rkt")

(struct program-result (exit-code error-info stdout stderr))
(define-namespace-anchor anchor)

(define (run-program modules signatures module-name main-name #:stdin stdin-bytes
                     #:args [supplied-args empty])
  (define racket-modules (racketize-modules modules signatures))
  (define ns (namespace-anchor->empty-namespace anchor))
  (parameterize ([current-namespace ns])
    (namespace-require 'racket/base))
  (define main-fun
    (eval
      `(begin
         (module prim racket/base
           (provide (all-defined-out))
           ,@(for/list ([(prim-name prim-val) (in-hash supported-primitives)])
               `(define ,prim-name ,prim-val)))
         ,@racket-modules
         (require
           (only-in ',(mod-name->racket-mod-name module-name)
             [,main-name main-fun]))
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

(define define-sym (datum->syntax #'define 'define))
(define app-sym (datum->syntax #'#%plain-app '#%plain-app))
(define lambda-sym (datum->syntax #'lambda 'lambda))
(define let-sym (datum->syntax #'let 'let))

;; patterns is a (Hash Symbol Symbol)
;; values is a (Hash Symbol Identifier)
;; statics is a (Hash Symbol varargs-bindings)
(struct environment (patterns values statics))
(struct varargs-bindings (cons-id empty-id))

(define (environment-set/value env var id)
  (match-define (environment pats vals statics) env)
  (environment pats (hash-set vals var id) statics))

(define (environment-ref/value env var f)
  (hash-ref (environment-values env) var f))

(define (environment-ref/static env var f)
  (hash-ref (environment-statics env) var f))


(define (racketize-modules modules signatures)
  (for/list ([module (topo-sort (set->list modules))])
    (racketize-module module signatures)))

(define (racketize-module module signatures)
  ;;mapping to identifiers
  (define local-env (make-hash))
  (define local-pattern-env (make-hash))
  ;;mapping to varargs-bindings
  (define local-static-env (make-hash))

  ;; TODO actually support more complicated pattern bindings
  (for ([imports (in-list (module&-imports module))])
    (match imports
      [(partial-imports& mod-name _ _ patterns _)
       (for ([import (in-list patterns)])
         (hash-set! local-pattern-env (import&-local-name import)
                    (import&-exported-name import)))]
      [(full-imports& mod-name)
       (match (hash-ref signatures mod-name)
         [(module-signature _ _ _ patterns _)
          (for ([export (in-hash-keys patterns)])
            (hash-set! local-pattern-env export export))])]))
  ;; Import forms use no lexical context as it is much faster.
  (define (null-context v) (datum->syntax #f v))
  (define (quoted-mod-name mod-name)
    (null-context `(,#'quote ,(mod-name->racket-mod-name mod-name))))
  (define module-import-forms
    (for/list ([imports (in-list (module&-imports module))])
      (match imports
        [(partial-imports& mod-name _ values _ _)
         #`(only-in #,(quoted-mod-name mod-name)
             #,@(for/list ([import (in-list values)])
                  (define id (generate-temporary (import&-local-name import)))
                  (hash-set! local-env (import&-local-name import) id)
                  `[,(null-context (import&-exported-name import)) ,id]))]
        [(full-imports& mod-name)
         #`(only-in #,(quoted-mod-name mod-name)
             #,@(match (hash-ref signatures mod-name)
                  [(module-signature _ values _ _ _)
                   (for/list ([export (in-hash-keys values)])
                     (define id (generate-temporary export))
                     (hash-set! local-env export id)
                     `[,(null-context export) ,id])]))])))
  (define static-import-forms
    (append*
      (for/list ([imports (in-list (module&-imports module))])
        (define static-names
          (match imports
            [(partial-imports& mod-name _ _ _ statics) statics]
            [(full-imports& mod-name)
             (match (hash-ref signatures mod-name)
               [(module-signature _ _ _ _ statics)
                (for/list ([export (in-hash-keys statics)])
                  (import& export export))])]))

        (define mod-name (imports&-module-name imports))
        (append*
          (for/list ([import static-names])
            (match (hash-ref (module-signature-statics (hash-ref signatures mod-name))
                             (import&-exported-name import))
              [(varargs-signature _ _ _ cons-mod cons-name empty-mod empty-name)
               (define cons-id (generate-temporary cons-name))
               (define empty-id (generate-temporary empty-name))
               (hash-set! local-static-env (import&-local-name import)
                          (varargs-bindings cons-id empty-id))
               (list
                 #`(only-in #,(quoted-mod-name cons-mod)
                     [#,(null-context cons-name) #,cons-id])
                 #`(only-in #,(quoted-mod-name empty-mod)
                     [#,(null-context empty-name) #,empty-id]))]))))))

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
                    #`(define #,constructor-id
                        (let ([v (variant-val '#,variant-name (vector))])
                          (lambda () v)))
                    #`(define (#,constructor-id . vs)
                        (variant-val '#,variant-name (vector . vs)))))
              (for/list ([field (variant&-fields variant)] [index (in-naturals)])
                (define field-name (variant-field&-name field))
                (define field-id (generate-temporary field-name))
                (hash-set! local-env
                  (string->symbol (format "~a-~a" variant-name field-name))
                  field-id)
                #`(define (#,field-id v)
                    (vector-ref (variant-val-fields v) #,index)))))))))


  (for ([(name _) (in-hash (module&-definitions module))])
    (define temporary (generate-temporary name))
    (hash-set! local-env name temporary))

  (for ([(name def) (in-hash (module&-static-defs module))])
    (hash-set!
      local-static-env
      name
      (match def
        [(varargs-definition& _ _ _ cons-sym empty-sym)
         (varargs-bindings
           (hash-ref local-env cons-sym)
           (hash-ref local-env empty-sym))])))

  (define function-defs
    (let ()
      (define immutable-local-env (hash-copy/immutable local-env))
      (define immutable-local-pattern-env (hash-copy/immutable local-pattern-env))
      (define immutable-local-static-env (hash-copy/immutable local-static-env))
      (for/list ([(name def) (in-hash (module&-definitions module))])
        (match def
          [(definition& _ args (block& defs body))
           (define temporaries (generate-temporaries args))
           `(,define-sym (,(hash-ref local-env name) ,@temporaries)
               ,(racketize-block
                  (environment
                    immutable-local-pattern-env
                    (for/fold ([env immutable-local-env])
                              ([a (in-list args)] [t (in-list temporaries)])
                      (hash-set env a t))
                    immutable-local-static-env)
                  defs
                  body))]))))

  (define racket-mod-name (mod-name->racket-mod-name (module&-name module)))
  (define racket-module
    #`(module #,racket-mod-name racket/base
        (require
          ;; Ensure that the machine-structs module is instantiated.
          (only-in (file #,(path->string machine-structs-path)))
          #,@module-import-forms
          #,@static-import-forms)

        #,@variant-defs
        #,@function-defs

        (provide
          (rename-out
            #,@(for/list ([export (in-list (exports&-values (module&-exports module)))])
                 (match-define (export& in-name out-name) export)
                 `[,(hash-ref local-env in-name) ,out-name])))))
  racket-module)


(define (racketize-block env defs body)
  (match defs
    [(list)
     (racketize-expr env body)]
    [(cons (match-def& pattern type expr) defs)
     (define vars (pattern-variables pattern))
     (define body-vars (generate-temporaries vars))
     (define body-env
       (for/fold ([env env]) ([var (in-list vars)] [id (in-list body-vars)])
         (environment-set/value env var id)))
     #`(let ([val #,(racketize-expr env expr)]
             [succ (#,lambda-sym (#,@body-vars) #,(racketize-block body-env defs body))])
         (#,app-sym #,(racketize-pattern pattern (environment-patterns env) vars)
          val succ (#,lambda-sym () (error 'match))))]))

;; env is hash table to expressions which evaluate to the value
(define (racketize-expr env expr)
  (match expr
    [(byte& v)
     `',v]
    [(bytes& v)
     `',v]
    [(boolean& v)
     `',v]
    [(variable& v)
     (environment-ref/value env v (lambda () (error 'racketize-expr "Unbound variables ~a" v)))]
    [(app& op args)
     `(,#'#%app
       ,@(for/list ([v (in-list (cons op args))])
           (racketize-expr env v)))]
    [(varargs-app& op args)
     `(,#'#%app
        ,(racketize-expr env op)
        (,#'#%app ,#'vector
                  ,@(for/list ([arg (in-list args)])
                      (racketize-expr env arg))))]
    [(varargs2-app& op args)
     (match-define (varargs-bindings cons-id empty-id)
       (environment-ref/static
         env op (lambda () (error 'racketize-expr "Unbound static variable ~a" op))))
     (for/fold ([acc `(,#'#%app ,empty-id)])
               ([arg (in-list (reverse args))])
       `(,#'#%app ,cons-id ,(racketize-expr env arg) ,acc))]

    [(if& cond true false)
     `(,#'if ,(racketize-expr env cond)
             ,(racketize-expr env true)
             ,(racketize-expr env false))]
    [(begin& first-expr exprs)
     `(,#'begin ,@(map (λ (e) (racketize-expr env e)) (cons first-expr exprs)))]
    [(ann& _ expr)
     (racketize-expr env expr)]
    [(let& name expr (block& defs body))
     (define compiled-expr (racketize-expr env expr))
     (cond
       [(identifier? compiled-expr)
        (racketize-block (environment-set/value env name compiled-expr) defs body)]
       [else
        (define temp (generate-temporary name))
        `(,#'let ([,temp ,compiled-expr])
            ,(racketize-block (environment-set/value env name temp) defs body))])]
    [(lambda& (list (list arg-names _) ...) _ (block& defs body))
     (define ids (generate-temporaries arg-names))
     (define new-env
       (for/fold ([env env]) ([name (in-list arg-names)] [id (in-list ids)])
         (environment-set/value env name id)))

     `(,#'lambda (,@ids) ,(racketize-block new-env defs body))]
    [(case& expr clauses)
     (define form
       (for/fold ([form #'(error 'end-of-case)]) ([clause (in-list (reverse clauses))])
         (match-define (case-clause& pattern (block& defs expr)) clause)
         (define vars (pattern-variables pattern))
         (define body-vars (generate-temporaries vars))
         (define body-env
           (for/fold ([env env]) ([var (in-list vars)] [id (in-list body-vars)])
             (environment-set/value env var id)))
         (define body (racketize-block body-env defs expr))
         #`(#,app-sym #,(racketize-pattern pattern (environment-patterns env) vars)
            val
            (#,lambda-sym (#,@body-vars) #,body)
            (#,lambda-sym () #,form))))

     #`(let ([val #,(racketize-expr env expr)]) #,form)]))

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

(define (mod-name->racket-mod-name mod-name)
  (match mod-name
    [(module-name& parts)
     (string->symbol
       (apply string-append
         (add-between
           (map symbol->string parts)
           "_")))]))
