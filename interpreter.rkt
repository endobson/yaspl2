#lang racket/base

(require
  "machine-structs.rkt"
  "type-structs.rkt"
  "signature-structs.rkt"
  "primitives.rkt"
  racket/list
  (only-in racket/contract/base and\/c)
  racket/hash
  racket/set
  racket/match)
(provide
  run-program
  module&-name
  parse-module
  check-module
  topo-sort
  construct-module-signature
  (struct-out program-result))

(struct module& (name imports exports types definitions))

(struct export& (name))
;; TODO change the order to match others
(struct imports& (types values patterns))
(struct import& (module-name name))
(struct definition& (type args body))

(struct expression& () #:transparent)
(struct byte& expression& (v) #:transparent)
(struct bytes& expression& (v) #:transparent)
(struct boolean& expression& (v) #:transparent)
(struct variable& expression& (v) #:transparent)
(struct if& expression& (cond true false) #:transparent)
(struct begin& expression& (first-expr exprs) #:transparent)
(struct app& expression& (op args) #:transparent)
(struct let& expression& (name expr body) #:transparent)
(struct case& expression& (expr clauses) #:transparent)
(struct case-clause& (variant-name field-variables expr) #:transparent)

(struct define-type& (type-name type-variables variants))
(struct variant& (name fields))
(struct variant-field& (name type))

(struct pre-type () #:transparent)
(struct var-pre-type pre-type (v) #:transparent)
(struct fun-pre-type pre-type (args result) #:transparent)
(struct type-app-pre-type pre-type (constructor args))

(struct pattern-spec (input-type type-vars field-types))



(define (parse-module sexp)
  (match sexp
    [`(module ,(? symbol? name)
        (import . ,(app parse-imports imports))
        (export . ,(app parse-exports exports))
        (types . ,(app parse-type-definitions types))
        . ,(app parse-definitions definitions))
     (module& name imports exports types definitions)]))

(define (parse-imports imports)
  (define (parse-imports imports)
    (for/lists (types vals patterns) ([import (in-list imports)])
      (parse-import import)))

  (define (parse-import import)
    (match import
      [(list (? symbol? module-name) (? symbol? function-names) ...)
       (values
         empty
         (for/list ([function-name (in-list function-names)])
           (import& module-name function-name))
         empty)]
      [`(,(? symbol? module-name)
          #:types (,(? symbol? type-names) ...)
          #:values (,(? symbol? function-names) ...)
          #:patterns (,(? symbol? pattern-names) ...))
       (values
         (for/list ([type-name (in-list type-names)])
           (import& module-name type-name))
         (for/list ([function-name (in-list function-names)])
           (import& module-name function-name))
         (for/list ([pattern-name (in-list pattern-names)])
           (import& module-name pattern-name)))]))


  (let-values ([(types vals patterns) (parse-imports imports)])
    (imports& (append* types) (append* vals) (append* patterns))))

(define (parse-exports exports)
  (match exports
   [(list (? symbol? exports) ...)
    (map export& exports)]))

(define (parse-type-definitions types)
  (define (parse-variant variant)
    (match variant
      [(list variant-name [list field-name (app parse-pre-type field-type)] ...)
       (variant& variant-name (map variant-field& field-name field-type))]))
  (define (parse-type-definition type)
    (match type
      [`(define-type ,(? symbol? type-name) . ,(list (app parse-variant variants) ...))
       (define-type& type-name #f variants)]
      [`(define-type ,(list (? symbol? type-name) (? symbol? type-variables) ..1)
                     . ,(list (app parse-variant variants) ...))
       (define-type& type-name type-variables variants)]))
  (map parse-type-definition types))


(define (parse-definitions defs)
  (define (parse-definition sexp)
    (match sexp
      [`(define (,name (,(? symbol? args) : ,arg-types) ...) : ,return-type ,body)
        (define type (fun-pre-type (map parse-pre-type arg-types) (parse-pre-type return-type)))
        (values name (definition& type args (parse-expression body)))]))
  (for/hash ([def (in-list defs)])
    (parse-definition def)))


(define (parse-expression sexp)
  (define parse parse-expression)
  (match sexp
    [(? byte? num) (byte& num)]
    [(? (and/c bytes? immutable?) bytes) (bytes& bytes)]
    [(? boolean? bool) (boolean& bool)]
    [(? symbol? sym) (variable& sym)]
    [`(if ,cond ,true ,false)
     (if& (parse cond) (parse true) (parse false))]
    [(list 'begin first-expr exprs ...)
     (begin& (parse first-expr) (map parse exprs))]
    [`(let ([,(? symbol? name) ,expr]) ,body)
     (let& name (parse expr) (parse body))]
    [`(case ,expr . ,(list (list (list (? symbol? variant-names) (? symbol? field-namess) ...) bodies) ...))
     (case& (parse expr) (map case-clause& variant-names field-namess (map parse bodies)))]
    [(list op args ...)
     (app& (parse op) (map parse args))]))

(define (parse-pre-type sexp)
  (match sexp
    [(? symbol?) (var-pre-type sexp)]
    [(list arg-types ... '-> result-type)
     (fun-pre-type (map parse-pre-type arg-types) (map parse-pre-type result-type))]
    [(list (? symbol? type-constructor) arg-types ...)
     (type-app-pre-type type-constructor (map parse-pre-type arg-types))]))



;; TODO ensure all exports have sensible bindings
(define (check-module module)
  (ensure-no-free-variables module))

;; TODO make this work over types and not conflate type bindings and value bindings
(define (ensure-no-free-variables module)
  (define ((recur/env env) expr)
    (define recur (recur/env env))
    (match expr
      [(? byte&?) (void)]
      [(? bytes&?) (void)]
      [(? boolean&?) (void)]
      [(variable& sym)
       (unless (set-member? env sym)
         (error 'ensure-no-free-variables "Unbound symbol '~a' in ~a" sym (module&-name module)))]
      [(if& cond true false)
       (for-each recur (list cond true false))]
      [(begin& first-expr exprs)
       (for-each recur (cons first-expr exprs))]
      [(app& op exprs)
       (for-each recur (cons op exprs))]
      [(let& name expr body)
       (recur expr)
       ((recur/env (set-add env name)) body)]
      [(case& expr (list (case-clause& _ field-varss bodies) ...))
       (recur expr)
       (for ([body (in-list bodies)]
             [field-vars (in-list field-varss)])
         ((recur/env (set-union env (list->set field-vars))) body))]))


  (match module
    [(module& _ (imports& _ (list (import& _ import-names) ...) _) _
       (list (define-type& _ _
               (list (variant& variant-namess (list (variant-field& field-namesss _) ...)) ...)) ...)
       definitions)


     (define mut-env (mutable-set))
     (for ([import-name (in-list import-names)])
       (set-add! mut-env import-name))
     (for ([variant-name (in-list (append* variant-namess))]
           [field-names (in-list (append* field-namesss))])
       (set-add! mut-env variant-name)
       (for ([field-name field-names])
         (set-add! mut-env (string->symbol (format "~a-~a" variant-name field-name)))))
     (for ([definition-name (in-hash-keys definitions)])
       (set-add! mut-env definition-name))

     (define env (set-union (set) mut-env))
     (for ([definition (in-hash-values definitions)])
       (match definition
         [(definition& _ args body)
          (let ([env (set-union env (list->set args))])
            ((recur/env env) body))]))]))


(define ((parse-type/env type-env) pre-type)
  (define parse-type (parse-type/env type-env))
  (match pre-type
    [(var-pre-type v) (hash-ref type-env v)]
    [(fun-pre-type args result)
     (fun-ty empty (map parse-type args) (parse-type result))]
    [(type-app-pre-type constructor args)
     ;; TODO check that args are the right kind
     (match (hash-ref type-env constructor)
       [(data-ty-constructor mod-name ty-name arg-kinds)
        (unless (= (length args) (length arg-kinds))
          (error 'parse-type "Type constructor applied to wrong number of arguments"))
        (data-ty mod-name ty-name (map parse-type args))])]))

(define (construct-module-signature module module-signatures)
  (match module
    [(module& module-name imports exports type-defs defs)
     (define type-name-env
       (hash-copy/immutable
         (let ([mut-type-name-env (make-hash)])
           (for ([type-def (in-list type-defs)])
             (match type-def
               [(define-type& type-name #f _)
                (hash-set! mut-type-name-env type-name (data-ty module-name type-name empty))]
               [(define-type& type-name (list (? symbol? type-vars) ...) _)
                (hash-set! mut-type-name-env type-name
                           (data-ty-constructor module-name type-name (map (λ (_) (*-kind)) type-vars)))]))
           (for ([import (in-list (imports&-types imports))])
             (match import
               [(import& src-mod name)
                ;; TODO make the prim module-signature work the same way as others
                (hash-set! mut-type-name-env name
                  (if (equal? src-mod 'prim)
                      (hash-ref (module-signature-exports (hash-ref module-signatures src-mod)) name)
                      (match (hash-ref (module-signature-types (hash-ref module-signatures src-mod)) name)
                        [(inductive-signature orig-mod-name ty-name #f variants)
                         (data-ty orig-mod-name ty-name empty)]
                        [(inductive-signature orig-mod-name ty-name type-vars variants)
                          (data-ty-constructor orig-mod-name ty-name
                                                       (map (λ (_) (*-kind)) type-vars))])))]))
           mut-type-name-env)))

     (define pattern-env
       (hash-copy/immutable
         (let ([mut-pattern-env (make-hash)])
           (for ([type-def (in-list type-defs)])
             (match type-def
               [(define-type& type-name #f variants)
                (for ([variant (in-list variants)])
                  (match variant
                    [(variant& name (list (variant-field& field-names field-types) ...))
                     (hash-set! mut-pattern-env name
                                (pattern-spec
                                  (hash-ref type-name-env type-name)
                                  empty
                                  ;; Make this real
                                  (map (λ (_) 'ty) field-types)))]))]
               [(define-type& type-name type-vars variants)
                (for ([variant (in-list variants)])
                  (match variant
                    [(variant& name (list (variant-field& field-names field-types) ...))
                     (hash-set! mut-pattern-env name
                                (pattern-spec
                                  ;; Apply this to the type vars
                                  (hash-ref type-name-env type-name)
                                  type-vars
                                  ;; Make this real
                                  (map (λ (_) 'ty) field-types)))]))]))
           mut-pattern-env)))


     (define mut-type-env (make-hash))


     (for ([type-def (in-list type-defs)])
       (match type-def
         [(define-type& type-name type-vars* variants)
          ;; TODO figure out how to handle this correctly
          (define type-vars (or type-vars* empty))
          (define defined-type (data-ty module-name type-name type-vars))
          (define parse-type
            (parse-type/env
              (hash-union
                type-name-env
                (for/hash ([type-var type-vars])
                  (values type-var (type-var-ty type-var))))))
          (for ([variant (in-list variants)])
             (match variant
               [(variant& variant-name (list (variant-field& field-names field-types) ...))
                (define parsed-field-types (map parse-type field-types))
                (hash-set! mut-type-env variant-name
                  (fun-ty type-vars parsed-field-types defined-type))
                (for ([field-name field-names]
                      [parsed-field-type parsed-field-types])
                  (hash-set! mut-type-env (string->symbol (format "~a-~a" variant-name field-name))
                             (fun-ty type-vars (list defined-type) parsed-field-type)))]))]))

     (define parse-type (parse-type/env type-name-env))

     (for ([(def-name def) (in-hash defs)])
       (match def
         [(definition& type _ _)
          (hash-set! mut-type-env def-name (parse-type type))]))

     (for ([import (in-list (imports&-values imports))])
       (match import
         [(import& src-mod name)
          (hash-set! mut-type-env name
                     (hash-ref (module-signature-exports (hash-ref module-signatures src-mod))
                               name))]))



     (define type-env (hash-copy/immutable mut-type-env))

     (for ([(def-name def) (in-hash defs)])
       (match def
         [(definition& _ args body)
          (match (hash-ref type-env def-name)
            [(fun-ty type-vars arg-types result-type)
             (let ([values (foldl (λ (k v h) (hash-set h k v)) type-env args arg-types)])
               ((type-check/env (binding-env values (hash) pattern-env)) body result-type))])]))

     ;; TODO limit this to only exported values not types
     (define exported-value-bindings
       (for/hash ([export exports])
         (define name (export&-name export))
         (values name (hash-ref type-env name (void-ty)))))

     ;; TODO limit this to the exported types
     ;; TODO add variants
     (define exported-type-bindings
       (for/hash ([type-def (in-list type-defs)])
         (match type-def
           [(define-type& type-name type-vars variants)
            (values
              type-name
              (inductive-signature module-name type-name type-vars empty))])))

     ;; TODO limit this to the exported patterns
     (define exported-pattern-bindings
       (for/fold ([acc (hash)]) ([type-def (in-list type-defs)])
         (hash-union acc
           (match type-def
             [(define-type& type-name type-vars variants)
              (for/hash ([variant (in-list variants)])
                (define name (variant&-name variant))
                (values
                  name
                  (hash-ref pattern-env name)))]))))

     (module-signature
       module-name
       exported-value-bindings
       exported-type-bindings
       exported-pattern-bindings)]))

(struct binding-env (values types patterns))

(define (binding-env-value-ref env name)
  (hash-ref (binding-env-values env) name))

(define (binding-env-value-set env name ty)
  (match env
    [(binding-env v t p)
     (binding-env (hash-set v name ty) t p)]))



(define ((type-check/env env) expr type)
  (define type-check (type-check/env env))
  (define type-infer (type-infer/env env))

  (define (check actual-type [expected-type type])
    (unless (bottom-ty? actual-type)
      (unless (equal? actual-type expected-type)
        (error 'type-check "Types don't match: Got ~s but expected ~s in ~s"
               actual-type expected-type expr))))
  (match expr
    [(byte& _) (check (byte-ty))]
    [(bytes& _) (check (bytes-ty))]
    [(boolean& _) (check (boolean-ty))]
    [(variable& v)
     (check (binding-env-value-ref env v))]
    [(if& cond true false)
     (type-check cond (boolean-ty))
     (type-check true type)
     (type-check false type)]
    [(begin& first-expr exprs)
     (match (cons first-expr exprs)
       [(list exprs ... last-expr)
        (for-each (λ (e) (type-check e (void-ty))) exprs)
        (type-check last-expr type)])]
    [(app& op args)
     (match (type-infer op)
       [(fun-ty type-vars arg-types body-type)
        (unless (equal? (length arg-types) (length args))
          (error 'type-check "Cannot apply function: Got ~s but expected ~s arguments"
                 (length arg-types)
                 (length args)))
        (cond
          [(empty? type-vars)
           (for ([arg (in-list args)] [arg-type (in-list arg-types)])
             (type-check arg arg-type))
           (check body-type)]
          ;; TODO figure out how to support functions with type variables
          [else (void)])])]
    [(let& name expr body)
     (let* ([expr-type (type-infer expr)]
            [type-env (binding-env-value-set env name expr-type)])
       ((type-check/env type-env) body type))]
    [(case& expr clauses)
     (type-infer expr)
     ;; TODO actually do this
     (check type)]))

;; TODO actually do this
(define ((type-infer/env env) expr)
  (define type-check (type-check/env env))
  (define type-infer (type-infer/env env))

  (match expr
    [(byte& _) (byte-ty)]
    [(bytes& _) (bytes-ty)]
    [(boolean& _) (boolean-ty)]
    [(variable& v)
     (binding-env-value-ref env v)]
    [(if& cond true false)
     (type-check cond (boolean-ty))
     (type-infer true)
     (type-infer false)]
    [(begin& first-expr exprs)
     (match (cons first-expr exprs)
       [(list exprs ... last-expr)
        (for-each (λ (e) (type-check e (void-ty))) exprs)
        (type-infer last-expr)])]
    [(app& op args)
     (match (type-infer op)
       [(fun-ty type-vars arg-types body-type)
        (unless (equal? (length arg-types) (length args))
          (error 'type-check "Cannot apply function: Got ~s but expected ~s arguments"
                 (length arg-types)
                 (length args)))
        (cond
          [(empty? type-vars)
           (for ([arg (in-list args)] [arg-type (in-list arg-types)])
             (type-check arg arg-type))
           body-type]
          ;; TODO figure out how to support functions with type variables
          [else (error 'type-infer "NYI app")])])]
    [(let& name expr body)
     (let* ([expr-type (type-infer expr)]
            [type-env (binding-env-value-set env name expr-type)])
       ((type-infer/env type-env) body))]
    [(case& expr clauses)
     ;; TODO actually do this
     (error 'type-infer "NYI case")]))




;;;;



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

  (define return-val (run-machine (call-function main-fun args (halt-k))))
  (program-result
    (if (error-sentinal? return-val) 255 (byte-val-v return-val))
    (and (error-sentinal? return-val) (error-sentinal-info return-val))
    (get-output-bytes stdout)
    (get-output-bytes stderr)))

(struct eval-machine-state (expr env cont))
(struct apply-machine-state (vals exprs env cont))
(struct cont-machine-state (val cont))
(struct error-machine-state (info))



(define (call-function fun args cont)
  (match fun
    [(function-val arg-names env body)
     (if (= (length args) (length arg-names))
         (let ([new-env
                 (for/fold ([env (hash-copy/immutable env)])
                           ([v (in-list args)] [name (in-list arg-names)])
                   (hash-set env name v))])
           (eval-machine-state body new-env cont))
         (error-machine-state
           (string->bytes/utf-8
             (format "Wrong number of arguments: Expected ~a, got ~a"
                     (length arg-names) (length args)))))]
    [(variant-constructor-val variant-name fields)
     (unless (= (length args) (length fields))
       (error 'variant-constructor "Wrong number of arguments for ~a: Expected ~a, got ~a"
              variant-name (length fields) (length args)))
     (cont-machine-state (variant-val variant-name args) cont)]
    [(field-accessor-val variant-name index)
     (match args
       [(list (variant-val (== variant-name equal?) fields))
        (cont-machine-state (list-ref fields index) cont)]
       [_
         (error-machine-state #"Wrong variant")])]
    [(prim-function-val name)
     (match (run-primitive name args)
       [(? value? val) (cont-machine-state val cont)]
       [(error-sentinal info) (error-machine-state info)])]))

(define (hash-copy/immutable env)
  (make-immutable-hash (hash->list env)))


(define (run-machine machine)
  (match machine
    [(error-machine-state info)
     (error-sentinal info)]
    [(eval-machine-state expr env cont)
     (match expr
       [(byte& v)
        (run-machine (cont-machine-state (byte-val v) cont))]
       [(bytes& v)
        (run-machine (cont-machine-state (bytes-val v) cont))]
       [(boolean& v)
        (run-machine (cont-machine-state (boolean-val v) cont))]
       [(variable& v)
        (define val (hash-ref env v #f))
        (run-machine
          (if val
              (cont-machine-state val cont)
              (error-machine-state (string->bytes/utf-8 (format "No binding for ~a available" v)))))]
       [(app& op vs)
        (run-machine (apply-machine-state empty (cons op vs) env cont))]
       [(if& cond true false)
        (run-machine (eval-machine-state cond env (if-k true false env cont)))]
       [(begin& first-expr exprs)
        (run-machine
          (eval-machine-state first-expr env
            (for/fold ([cont cont]) ([expr (in-list (reverse exprs))])
              (ignore-k expr env cont))))]
       [(case& expr clauses)
        (run-machine
          (eval-machine-state expr env (case-k clauses env cont)))]
       [(let& name expr body)
        (run-machine
          (eval-machine-state expr env
            (bind-k name body env cont)))])]
    [(apply-machine-state vals exprs env cont)
     (run-machine
       (if (empty? exprs)
           (let ([vals (reverse vals)])
             (call-function (first vals) (rest vals) cont))
           (eval-machine-state
             (first exprs)
             env
             (apply-k vals (rest exprs) env cont))))]
    [(cont-machine-state val cont)
     (match cont
       [(halt-k) val]
       [(ignore-k expr env cont)
        (run-machine (eval-machine-state expr env cont))]
       [(apply-k vals args env cont)
        (run-machine (apply-machine-state (cons val vals) args env cont))]
       [(if-k true false env cont)
        (define expr (if (boolean-val-v val) true false))
        (run-machine (eval-machine-state expr env cont))]
       [(case-k clauses env cont)
        (match val
          [(variant-val variant-name _)
           (define clause
             (for/first ([clause (in-list clauses)]
                         #:when (equal? (case-clause&-variant-name clause) variant-name))
               clause))
           (unless clause
             (error 'case "No match for ~a in ~a"
                    variant-name (map case-clause&-variant-name clauses)))
           (define new-env
             (for/fold ([env env]) ([arg-name (in-list (case-clause&-field-variables clause))]
                                    [field-val (in-list (variant-val-fields val))])
               (hash-set env arg-name field-val)))

           (run-machine (eval-machine-state (case-clause&-expr clause) new-env cont))])]
       [(bind-k name body env cont)
        (run-machine (eval-machine-state body (hash-set env name val) cont))])]))

