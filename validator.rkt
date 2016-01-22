#lang racket/base

(require
  "type-structs.rkt"
  "parser-structs.rkt"
  "signature-structs.rkt"
  "utils.rkt"
  racket/set
  racket/hash
  racket/list
  racket/match)

(provide
  check-module
  construct-module-signature)


(struct pattern-spec (input-type type-vars field-types))

;; TODO ensure all exports have sensible bindings
(define (check-module module)
  (ensure-no-free-variables module))

;; TODO make this work over types and not conflate type bindings and value bindings
;; TODO also support patterns
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
      [(case& expr clauses)
       (for ([clause (in-list clauses)])
         (define (pattern-binding-variables p acc)
           (match p
             [(bytes-pattern& _) acc]
             [(ignore-pattern&) acc]
             [(variable-pattern& v) (cons v acc)]
             [(abstraction-pattern& name patterns)
              (for/fold ([acc acc]) ([pattern (in-list patterns)])
                (pattern-binding-variables pattern acc))]))
         (match clause
           [(case-clause& pattern expr)
            (define binders (pattern-binding-variables pattern empty))
            (when (check-duplicates binders)
              (error 'ensure-no-free-variables "Duplicate binder in ~a" binders))
            ((recur/env (set-union env (list->set binders))) expr)]))]))


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
    [(fun-pre-type type-vars args result)
     (define parse-type
       (parse-type/env
        (for/fold ([env type-env]) ([tv (in-list type-vars)])
          (hash-set env tv (type-var-ty tv)))))
     (fun-ty type-vars (map parse-type args) (parse-type result))]
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

     (define mut-type-env (make-hash))

     (define variant-parsed-field-types (make-hash))

     (for ([type-def (in-list type-defs)])
       (match type-def
         [(define-type& type-name type-vars* variants)
          ;; TODO figure out how to handle this correctly
          (define type-vars (or type-vars* empty))
          (define defined-type (data-ty module-name type-name (map type-var-ty type-vars)))
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
                (hash-set! variant-parsed-field-types variant-name parsed-field-types)
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
                                  (hash-ref variant-parsed-field-types name)))]))]
               [(define-type& type-name type-vars variants)
                (for ([variant (in-list variants)])
                  (match variant
                    [(variant& name (list (variant-field& field-names field-types) ...))
                     (match-define (data-ty-constructor ty-module-name ty-name _)
                       (hash-ref type-name-env type-name))
                     (hash-set! mut-pattern-env name
                                (pattern-spec
                                  (data-ty ty-module-name ty-name (map type-var-ty type-vars))
                                  type-vars
                                  (hash-ref variant-parsed-field-types name)))]))]))
          (for ([import (in-list (imports&-patterns imports))])
            (match import
              [(import& src-mod name)
               (hash-set!
                 mut-pattern-env
                 name
                 (hash-ref (module-signature-patterns (hash-ref module-signatures src-mod)) name))]))
           mut-pattern-env)))


     ;; TODO add types for variants
     (define module-algebraic-types
       (for/list ([type-def (in-list type-defs)])
         (match type-def
           [(define-type& type-name type-vars variants)
            (inductive-signature module-name type-name type-vars
              (for/list ([variant (in-list variants)])
                (variant-signature (variant&-name variant)
                                   (map (λ (_) 'nyi) (variant&-fields variant)))))])))

     ;; TODO remove hack to limit imports
     (define imported-algebraic-types
       (for/fold ([acc empty]) ([module-signature (in-hash-values module-signatures)])
         (match-define (imports& (list (import& t-mods _) ...)
                                 (list (import& v-mods _) ...)
                                 (list (import& p-mods _) ...)) imports)
         (if (member (module-signature-name module-signature)
                     (append t-mods v-mods p-mods))
             (append (hash-values (module-signature-types module-signature)) acc)
             acc)))

     (define algebraic-types
       (append module-algebraic-types imported-algebraic-types))


     (for ([(def-name def) (in-hash defs)])
       (match def
         [(definition& _ args body)
          (match (hash-ref type-env def-name)
            [(fun-ty type-vars arg-types result-type)
             (let ([values (foldl (λ (k v h) (hash-set h k v)) type-env args arg-types)])
               ((type-check/env (binding-env values algebraic-types pattern-env)) body result-type))])]))

     ;; TODO limit this to only exported values not types
     (define exported-value-bindings
       (for/hash ([export exports])
         (define name (export&-name export))
         (values name (hash-ref type-env name (void-ty)))))

     ;; TODO define a mechanism for exporting types and limit this to the exported types
     (define exported-type-bindings
       (for/hash ([ind-sig (in-list module-algebraic-types)])
         (values (inductive-signature-name ind-sig) ind-sig)))



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

(define (binding-env-pattern-ref env name)
  (hash-ref (binding-env-patterns env) name))

;; Finds a substitution of types for type-vars so that if every type variable instance was replaced by
;; its correspending type then all the pairs would be equal.
(define (unify-types type-vars type-pairs-list)
  (define (check-type-map type-map)
    (for/fold ([type-map type-map]) ([tv (in-list type-vars)])
      (if (hash-has-key? type-map tv)
          type-map
          (hash-set type-map tv 'unused-type-var))))


  (define ((replace v new-t) t)
    (define (replace t)
      (match t
        [(type-var-ty v2)
         (if (equal? v v2) new-t t)]
        [(data-ty mod-name name args)
         (data-ty mod-name name (map replace args))]
        [(void-ty) t]
        [(byte-ty) t]
        [(bytes-ty) t]
        [(boolean-ty) t]
        [(input-port-ty) t]
        [(output-port-ty) t]))
    (replace t))

  (define ((replace-pair v new-t) pair)
    (map (replace v new-t) pair))

  (define (occurs-check v t)
    (define (check t)
      (match t
        [(type-var-ty v2)
         (when (equal? v v2)
           (error 'occurs-check))]
        [(data-ty mod-name name args)
         (for-each check args)]
        [(void-ty) (void)]
        [(byte-ty) (void)]
        [(bytes-ty) (void)]
        [(boolean-ty) (void)]
        [(input-port-ty) (void)]
        [(output-port-ty) (void)]))
    (check t))


  (let loop ([type-map (hash)] [pairs type-pairs-list])
    (define (add-to-type-map var type)
      (when (hash-ref type-map var #f)
        (error 'unify-types "Cannot unify ~s twice" var))
      (hash-set
        (for/hash ([(k v) (in-hash type-map)])
          (values k ((replace var type) v)))
        var
        type))

    (match pairs
      [(list) (check-type-map type-map)]
      [(cons (list l r) pairs)
       (match* (l r)
         [((type-var-ty lv) (type-var-ty rv))
          #:when (equal? lv rv)
          (loop type-map pairs)]
         [((type-var-ty (? (λ (v) (member v type-vars)) v)) r)
          (occurs-check v r)
          (loop (add-to-type-map v r)
                (map (replace-pair v r) pairs))]
         [(l (type-var-ty (? (λ (v) (member v type-vars)) v)))
          (occurs-check v l)
          (loop (add-to-type-map v l)
                (map (replace-pair v l) pairs))]
         ;; TODO support for function types
         ;; TODO support for inductive types
         [((void-ty) (void-ty))
          (loop type-map pairs)]
         [((byte-ty) (byte-ty))
          (loop type-map pairs)]
         [((bytes-ty) (bytes-ty))
          (loop type-map pairs)]
         [((boolean-ty) (boolean-ty))
          (loop type-map pairs)]
         [((input-port-ty) (input-port-ty))
          (loop type-map pairs)]
         [((output-port-ty) (output-port-ty))
          (loop type-map pairs)]
         [((data-ty mod-name-l name-l args-l) (data-ty mod-name-r name-r args-r))
          #:when (and (equal? mod-name-l mod-name-r) (equal? name-l name-r))
          (loop type-map (append (map list args-l args-r) pairs))])])))

(define (substitute type-map t)
  (define sub (λ (t) (substitute type-map t)))
  (match t
    ;; TODO support the rest of the primitive types
    [(type-var-ty v)
     (define res (hash-ref type-map v t))
     (when (equal? res 'unused-type-var)
       (error 'substitute "Attempting to use a type variable that wasn't constrained: ~s" v))
     res]
    [(void-ty) t]
    [(byte-ty) t]
    [(bytes-ty) t]
    [(boolean-ty) t]
    [(input-port-ty) t]
    [(output-port-ty) t]

    [(data-ty module-name name types)
     (data-ty module-name name (map sub types))]))




(define ((type-check/env env) expr type)
  (define type-check (type-check/env env))
  (define (type-infer expr) ((type-check/env env) expr (unknown-ty)))

  (define (check actual-type [expected-type type])
    (cond
      [(unknown-ty? expected-type)
       actual-type]
      [else
       (unless (equal? actual-type expected-type)
         (error 'type-check "Types don't match: Got ~s but expected ~s in ~s"
                actual-type expected-type expr))]))
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
     ;; TODO make this use the type information on the op instead
     (match (type-infer op)
       [(fun-ty stale-type-vars stale-arg-types stale-body-type)
        (match-define-values (type-vars (cons body-type arg-types))
          (freshen-types stale-type-vars (cons stale-body-type stale-arg-types)))
        (unless (equal? (length arg-types) (length args))
          (error 'type-check "Cannot apply function: Got ~s but expected ~s arguments"
                 (length args)
                 (length arg-types)))
        (cond
          [(unknown-ty? type)
           (cond
             [(empty? type-vars)
              (for ([arg (in-list args)] [arg-type (in-list arg-types)])
                (type-check arg arg-type))
              (check body-type)]
             [else
               (check
                 (substitute
                   (unify-types type-vars (map list arg-types (map type-infer args)))
                   body-type))])]
          [else
           (define body-substitution (unify-types type-vars (list (list body-type type))))
           (if (member 'unused-type-var (hash-values body-substitution))
               (check
                 (substitute
                   (unify-types type-vars (map list arg-types (map type-infer args)))
                   body-type))
               (for ([arg (in-list args)] [arg-type (in-list arg-types)])
                 (type-check arg (substitute body-substitution arg-type)))) ])])]
    [(let& name expr body)
     (let* ([expr-type (type-infer expr)]
            [type-env (binding-env-value-set env name expr-type)])
       ((type-check/env type-env) body type))]
    [(case& expr clauses)
     (check-patterns-complete/not-useless env (map case-clause&-pattern clauses))


     (define expr-type (type-infer expr))

     (define types
       (for/set ([clause (in-list clauses)])
         (match clause
           [(case-clause& pattern body)
            ;; The variables are already fresh
            (match-define (template-data type-vars var-types constraints pattern-type)
              ((pattern->template-data/env env) pattern))

            (define substitution
              (unify-types type-vars (cons (list pattern-type expr-type) constraints)))


            (define new-env
              (for/fold ([env env]) ([(field-var field-type) (in-hash var-types)])
                (binding-env-value-set env field-var (substitute substitution field-type))))

            ((type-check/env new-env) body type)])))


     (when (unknown-ty? type)
       (unless (= (set-count types) 1)
         (error 'type-infer "Case clauses have conflicting result types"))
       (set-first types))]))

(define (fresh-ty-var sym)
  (gensym sym))

(define (freshen-types stale-vars stale-types)
  (define fresh-ty-vars (map fresh-ty-var stale-vars))
  (define type-map (make-immutable-hash (map cons stale-vars (map type-var-ty fresh-ty-vars))))
  (values
    fresh-ty-vars
    (map (λ (t) (substitute type-map t)) stale-types)))


(struct template-data (type-vars var-types constraints type))

(define ((pattern->template-data/env env) pattern)
  (define pattern->template-data (pattern->template-data/env env))
  (match pattern
    [(bytes-pattern& _) (template-data empty (hash) empty (bytes-ty))]
    [(variable-pattern& v)
     (define tv (fresh-ty-var v))
     (template-data (list tv) (hash v (type-var-ty tv)) empty (type-var-ty tv))]
    [(ignore-pattern&)
     (define tv (fresh-ty-var '_))
     (template-data (list tv) (hash) empty (type-var-ty tv))]
    [(abstraction-pattern& name patterns)
     (define templates (map pattern->template-data patterns))
     (define pattern-spec (binding-env-pattern-ref env name))
     (define-values (ty-vars types)
       (freshen-types
         (pattern-spec-type-vars pattern-spec)
         (cons (pattern-spec-input-type pattern-spec)
               (pattern-spec-field-types pattern-spec))))
     (match-define (cons type field-types) types)
     (define constraints
       (map list field-types (map template-data-type templates)))

     (template-data
       (append* ty-vars (map template-data-type-vars templates))
       (apply hash-union (hash) (map template-data-var-types templates))
       (append* constraints (map template-data-constraints templates))
       type)]))


(struct any-abstract-value () #:transparent)
;; This is some part of the space that isn't fully matched. Usually a literal.
;; Should only be used in the return values on the matched side.
(struct some-abstract-value () #:transparent)
(struct abstract-variant (name fields) #:transparent)


(define (check-patterns-complete/not-useless env patterns)

  (define (lookup-variants variant)
    (define ind-sigs
      (for*/list ([ind-sig (in-list (binding-env-types env))]
                  #:when (member variant (map variant-signature-name
                                           (inductive-signature-variants ind-sig))))
        ind-sig))
    ;; TODO avoid this issue
    (unless (= (length ind-sigs) 1)
      (error 'lookup-other-variants "Bad variant ~s: ~s" variant ind-sigs))
    (define abstract-values
      (for/hash ([variant-sig (in-list (inductive-signature-variants (first ind-sigs)))])
        (define name (variant-signature-name variant-sig))
        (values
          name
          (abstract-variant name (map (λ (_) (any-abstract-value))
                                      (variant-signature-types variant-sig))))))
    (values
      (hash-ref abstract-values variant)
      (list->set
        (hash-values (hash-remove abstract-values variant)))))

  (define (abstract-match/many pattern abstract-values)
    (for/fold ([leftovers-acc (set)] [matched-acc (set)])
              ([abstract-value (in-set abstract-values)])
      (define-values (leftovers matched)
        (abstract-match pattern abstract-value))
      (values
        (set-union leftovers-acc leftovers)
        (set-union matched-acc matched))))


  (define (abstract-match/vec patterns abstract-values)
    (match* (patterns abstract-values)
      [((list) (list))
       (values (set) (set (list)))]
      [((cons pattern patterns) (cons abstract-value abstract-values))
       (define-values (unmatched matched)
         (abstract-match pattern abstract-value))

       (define unmatched-unmatched
         (for/set ([unmatched-value (in-set unmatched)])
           (cons unmatched-value abstract-values)))

       (define-values (matched-unmatched matched-matched)
         (if (set-empty? matched)
             (values (set) (set))
             (let-values ([(rec-unmatched rec-matched)
                           (abstract-match/vec patterns abstract-values)])
               (values
                 (for*/set ([matched-value (in-set matched)]
                            [unmatched-value-list (in-set rec-unmatched)])
                   (cons matched-value unmatched-value-list))
                 (for*/set ([matched-value (in-set matched)]
                            [matched-value-list (in-set rec-matched)])
                   (cons matched-value matched-value-list))))))
       (values
         (set-union unmatched-unmatched matched-unmatched)
         matched-matched)]))

  (define (abstract-match pattern abstract-value)
    (match pattern
      [(bytes-pattern& _) (values (set abstract-value) (set (some-abstract-value)))]
      [(or (variable-pattern& _) (ignore-pattern&))
       (values (set) (set abstract-value))]
      [(abstraction-pattern& pat-name patterns)
       (match abstract-value
        [(any-abstract-value)
         ;; Compute all variants
         (define-values (refined-value other-variant-values)
           (lookup-variants pat-name))
         (define-values (unmatched matched)
           (abstract-match pattern refined-value))
         (values
           (set-union unmatched other-variant-values)
           matched)]
        [(abstract-variant val-name fields)
         #:when (equal? pat-name val-name)
         (define-values (unmatched-vecs matched-vecs)
           (abstract-match/vec patterns fields))
         (values
           (for/set ([unmatched-vec (in-set unmatched-vecs)])
              (abstract-variant val-name unmatched-vec))
           (for/set ([matched-vec (in-set matched-vecs)])
              (abstract-variant val-name matched-vec)))]
        [(abstract-variant _ _)
         (values (set abstract-value) (set))])]))

  (define unmatched
    (for/fold ([incoming-abstract-values (set (any-abstract-value))])
              ([pattern (in-list patterns)])
      (define-values (unmatched matched)
        (abstract-match/many pattern incoming-abstract-values))
      (when (set-empty? matched)
        (error 'pattern-match "Unmatchable pattern: ~s" pattern))
      unmatched))
  (unless (set-empty? unmatched)
    (error 'pattern-match "Unmatched values: ~s in ~s" unmatched patterns)))



