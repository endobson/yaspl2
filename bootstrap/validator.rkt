#lang racket/base

(require
  "type-structs.rkt"
  "parser-structs.rkt"
  "signature-structs.rkt"
  "utils.rkt"
  racket/set
  racket/string
  racket/hash
  racket/list
  racket/match)

(provide
  check-module
  construct-module-signature)


(struct pattern-spec (variant-name input-type type-vars field-types) #:transparent)

;; TODO ensure all exports have sensible bindings
(define (check-module module signatures)
  (ensure-no-free-variables module signatures))

(define (pattern-binding-variables p)
  (define (recur p acc)
    (match p
      [(bytes-pattern& _) acc]
      [(byte-pattern& _) acc]
      [(ignore-pattern&) acc]
      [(variable-pattern& v) (cons v acc)]
      [(abstraction-pattern& name patterns)
       (for/fold ([acc acc]) ([pattern (in-list patterns)])
         (recur pattern acc))]))
  (recur p empty))


;; TODO make this work over types and not conflate type bindings and value bindings
;; TODO also support patterns
(define (ensure-no-free-variables module signatures)
  (define unbound (mutable-set))
  (define ((recur/env env) expr)
    (define recur (recur/env env))
    (match expr
      [(? byte&?) (void)]
      [(? bytes&?) (void)]
      [(? boolean&?) (void)]
      [(variable& sym)
       (unless (set-member? env sym)
         (set-add! unbound sym))]
      [(if& cond true false)
       (for-each recur (list cond true false))]
      [(begin& first-expr exprs)
       (for-each recur (cons first-expr exprs))]
      [(app& op exprs)
       (for-each recur (cons op exprs))]
      [(varargs-app& op exprs)
       (for-each recur (cons op exprs))]
      [(let& name expr body)
       (recur expr)
       ((recur/env (set-add env name)) body)]
      [(ann& _ expr)
       (recur expr)]
      [(lambda& (list (list args _) ...) _ (block& defs body))
       (recur/block (set-union env (list->set args)) defs body)]
      [(case& expr clauses)
       (recur expr)
       (for ([clause (in-list clauses)])
         (match clause
           [(case-clause& pattern (block& defs expr))
            (define binders (pattern-binding-variables pattern))
            (when (check-duplicates binders)
              (raise-user-error 'ensure-no-free-variables "Duplicate binder in ~a" binders))
            (recur/block (set-union env (list->set binders)) defs expr)]))]))
  (define (recur/block env defs body)
    (match defs
      [(list) ((recur/env env) body)]
      [(cons (match-def& pattern type expr) defs)
       ((recur/env env) expr)
       (define binders (pattern-binding-variables pattern))
       (when (check-duplicates binders)
         (raise-user-error 'ensure-no-free-variables "Duplicate binder in ~a" binders))
       (recur/block (set-union env (list->set binders)) defs body)]))

  (match module
    [(module& _ importss  _
       (list (define-type& _ _
               (list (variant& variant-namess (list (variant-field& field-namesss _) ...)) ...)) ...)
       definitions)

     (define mut-env (mutable-set))
     (for ([imports (in-list importss)])
       (match imports
         [(partial-imports& _ _ import-names _)
          (for ([import-name (in-list import-names)])
            (set-add! mut-env (import&-local-name import-name)))]
         [(full-imports& mod-name)
          (match (hash-ref signatures mod-name)
            [(module-signature _ exports _ _)
             (for ([export (in-hash-keys exports)])
               (set-add! mut-env export))])]))
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
         [(definition& _ args (block& defs body))
          (let ([env (set-union env (list->set args))])
            (recur/block env defs body))]))])
  (unless (set-empty? unbound)
    (raise-user-error
      'ensure-no-free-variables "The following symbols are unbound in ~a:\n~a"
      (module&-name module) (string-join (map (λ (sym) (format "  ~a" sym)) (set->list unbound)) "\n"))))


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
          (raise-user-error 'parse-type "Type constructor applied to wrong number of arguments"))
        (data-ty mod-name ty-name (map parse-type args))]
       [(array-ty-constructor)
        (unless (= (length args) 1)
          (raise-user-error 'parse-type "Array type constructor applied to wrong number of arguments"))
        (array-ty (parse-type (first args)))])]))

(define (construct-module-signature module module-signatures)
  (match module
    [(module& module-name importss exports type-defs defs)
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
           (for ([imports (in-list importss)])
             (define import-names
               (match imports
                 [(partial-imports& _ types _ _) types]
                 [(full-imports& mod-name)
                  (match (hash-ref module-signatures mod-name)
                    [(module-signature _ _ types _)
                     (for/list ([export (in-hash-keys types)])
                       (import& export export))])]))
             (for ([import (in-list import-names)])
               (define src-mod (imports&-module-name imports))
               (match import
                 [(import& exported-name local-name)
                  (hash-set! mut-type-name-env local-name
                    (match (hash-ref (module-signature-types (hash-ref module-signatures src-mod))
                                     exported-name
                                     (lambda ()
                                       (raise-user-error
                                         'validator
                                         "Module ~s doesn't have exported type ~a"
                                         src-mod exported-name)))
                      [(prim-signature ty) ty]
                      [(inductive-signature orig-mod-name ty-name #f variants)
                       (data-ty orig-mod-name ty-name empty)]
                      [(inductive-signature orig-mod-name ty-name type-vars variants)
                        (data-ty-constructor orig-mod-name ty-name
                                                     (map (λ (_) (*-kind)) type-vars))]))])))
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

     (for ([imports (in-list importss)])
       (define import-names
         (match imports
           [(partial-imports& _ _ values _) values]
           [(full-imports& mod-name)
            (match (hash-ref module-signatures mod-name)
              [(module-signature _ values _ _)
               (for/list ([export (in-hash-keys values)])
                 (import& export export))])]))
       (define src-mod (imports&-module-name imports))
       (for ([import (in-list import-names)])
         (match import
           [(import& exported-name local-name)
            (hash-set! mut-type-env local-name
                       (hash-ref (module-signature-exports (hash-ref module-signatures src-mod))
                                 exported-name
                                 (lambda ()
                                   (raise-user-error
                                     'validator
                                     "Error validating ~s: Module ~s doesn`t have exported value ~a"
                                     module-name src-mod exported-name))))])))



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
                                  name
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
                                  name
                                  (data-ty ty-module-name ty-name (map type-var-ty type-vars))
                                  type-vars
                                  (hash-ref variant-parsed-field-types name)))]))]))
          (for ([imports (in-list importss)])
            (define import-names
              (match imports
                [(partial-imports& _ _ _ patterns) patterns]
                [(full-imports& mod-name)
                 (match (hash-ref module-signatures mod-name)
                   [(module-signature _ _ _ patterns)
                    (for/list ([export (in-hash-keys patterns)])
                      (import& export export))])]))
            (for ([import (in-list import-names)])
              (define src-mod (imports&-module-name imports))
              (match import
                [(import& exported-name local-name)
                 (hash-set!
                   mut-pattern-env
                   local-name
                   (hash-ref (module-signature-patterns (hash-ref module-signatures src-mod)) exported-name
                             (lambda () (raise-user-error
                                          'import-pattern
                                          "No pattern '~s' exported by ~s. (Imported by ~s)"
                                          exported-name src-mod module-name))))])))
           mut-pattern-env)))


     ;; TODO add types for variants
     (define module-inductive-signatures
       (for/list ([type-def (in-list type-defs)])
         (match type-def
           [(define-type& type-name type-vars variants)
            (inductive-signature module-name type-name type-vars
              (for/list ([variant (in-list variants)])
                (variant-signature (variant&-name variant)
                                   (map (λ (_) 'nyi) (variant&-fields variant)))))])))

     ;; TODO remove hack to limit imports
     (define imported-inductive-signatures
       (for/fold ([acc empty]) ([module-signature (in-hash-values module-signatures)])
         (define mods (map imports&-module-name importss))
         (if (member (module-signature-name module-signature) mods)
             (append (hash-values (module-signature-types module-signature)) acc)
             acc)))

     (define inductive-signatures
       (filter
         inductive-signature?
         (append module-inductive-signatures imported-inductive-signatures)))


     (for ([(def-name def) (in-hash defs)])
       (match def
         [(definition& _ args (block& defs body))
          (match (hash-ref type-env def-name)
            [(fun-ty type-vars arg-types result-type)
             (let ([values (foldl (λ (k v h) (hash-set h k v)) type-env args arg-types)])
               (let ([type-name-env
                       (foldl (λ (k h) (hash-set h k (type-var-ty k))) type-name-env type-vars)])
                 (type-check/block
                  (binding-env values inductive-signatures pattern-env type-name-env)
                  defs body result-type)))])]))

     (define exported-value-bindings
       (for/hash ([export (exports&-values exports)])
         (match-define (export& in-name out-name) export)
         (values out-name (hash-ref type-env in-name (void-ty)))))

     (define exported-type-bindings
       (for/hash ([export (exports&-types exports)])
         (match-define (export& in-name out-name) export)
         (values
           out-name
           (or
             (for/first ([ind-sig (in-list inductive-signatures)]
                         #:when (equal? (inductive-signature-name ind-sig) in-name))
               ind-sig)
             (raise-user-error 'bad-export "No datatype with name ~a" in-name)))))

     (define exported-pattern-bindings
       (for/hash ([export (exports&-patterns exports)])
         (match-define (export& in-name out-name) export)
         (values out-name (hash-ref pattern-env in-name
                                    (lambda () (raise-user-error
                                                 'export-pattern
                                                 "No pattern '~s' exported by ~s"
                                                 in-name module-name))))))

     (module-signature
       module-name
       exported-value-bindings
       exported-type-bindings
       exported-pattern-bindings)]))

(struct binding-env (values inductive-signatures patterns types))

(define (binding-env-value-ref env name)
  (hash-ref (binding-env-values env) name))

(define (binding-env-value-set env name ty)
  (match env
    [(binding-env v is p t)
     (binding-env (hash-set v name ty) is p t)]))

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
        [(array-ty e)
         (array-ty (replace e))]
        [(void-ty) t]
        [(s8-ty) t]
        [(u8-ty) t]
        [(s32-ty) t]
        [(u32-ty) t]
        [(u64-ty) t]
        [(int-ty) t]
        [(bytes-ty) t]
        [(boolean-ty) t]
        [(fun-ty '() arg-tys result-ty)
         (fun-ty '() (map replace arg-tys) (replace result-ty))]
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
        [(fun-ty '() arg-tys result-ty)
         (for-each check arg-tys)
         (check result-ty)]
        [(array-ty e)
         (check  e)]
        [(void-ty) (void)]
        [(s8-ty) (void)]
        [(u8-ty) (void)]
        [(s32-ty) (void)]
        [(u32-ty) (void)]
        [(u64-ty) (void)]
        [(int-ty) (void)]
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
         [((void-ty) (void-ty))
          (loop type-map pairs)]
         [((s8-ty) (s8-ty))
          (loop type-map pairs)]
         [((u8-ty) (u8-ty))
          (loop type-map pairs)]
         [((s32-ty) (s32-ty))
          (loop type-map pairs)]
         [((u32-ty) (u32-ty))
          (loop type-map pairs)]
         [((u64-ty) (u64-ty))
          (loop type-map pairs)]
         [((int-ty) (int-ty))
          (loop type-map pairs)]
         [((bytes-ty) (bytes-ty))
          (loop type-map pairs)]
         [((boolean-ty) (boolean-ty))
          (loop type-map pairs)]
         [((input-port-ty) (input-port-ty))
          (loop type-map pairs)]
         [((output-port-ty) (output-port-ty))
          (loop type-map pairs)]
         [((array-ty l) (array-ty r))
          (loop type-map (cons (list l r) pairs))]
         [((fun-ty '() arg-tys-l result-ty-l) (fun-ty '() arg-tys-r result-ty-r))
          (loop type-map (cons (list result-ty-l result-ty-r)
                               (append (map list arg-tys-l arg-tys-r) pairs)))]
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
    [(s8-ty) t]
    [(u8-ty) t]
    [(s32-ty) t]
    [(u32-ty) t]
    [(u64-ty) t]
    [(int-ty) t]
    [(bytes-ty) t]
    [(boolean-ty) t]
    [(input-port-ty) t]
    [(output-port-ty) t]

    ;; Handle polymorhpic functions
    [(fun-ty '() arg-types result-type)
     (fun-ty '() (map sub arg-types) (sub result-type))]
    [(array-ty type)
     (array-ty (sub type))]
    [(data-ty module-name name types)
     (data-ty module-name name (map sub types))]))



(define (type-check/block env defs body type)
  (match defs
    [(list) ((type-check/env env) body type)]
    [(cons (match-def& pattern def-pre-type expr) defs)
     (check-patterns-complete/not-useless env (list pattern))
     (define expr-type
       (if def-pre-type
           (let ([def-type ((parse-type/env (binding-env-types env)) def-pre-type)])
             ((type-check/env env) expr def-type)
             def-type)
           ((type-check/env env) expr (unknown-ty))))

     (match-define (template-data type-vars var-types constraints pattern-type)
       ((pattern->template-data/env env) pattern))

     (define substitution
       (unify-types type-vars (cons (list pattern-type expr-type) constraints)))

     (define new-env
       (for/fold ([env env]) ([(field-var field-type) (in-hash var-types)])
         (binding-env-value-set env field-var (substitute substitution field-type))))
     (type-check/block new-env defs body type)]))


(define ((type-check/env env) expr type)
  (define type-check (type-check/env env))
  (define (type-infer expr) ((type-check/env env) expr (unknown-ty)))

  (define (check actual-type [expected-type type])
    (cond
      [(unknown-ty? expected-type)
       actual-type]
      [else
       (unless (equal? actual-type expected-type)
         (raise-user-error
           'type-check "Types don't match: Got ~s but expected ~s in ~s"
           actual-type expected-type expr))]))
  (match expr
    [(byte& _) (check (int-ty))]
    [(bytes& _) (check (bytes-ty))]
    [(boolean& _) (check (boolean-ty))]
    [(variable& v)
     (check (binding-env-value-ref env v))]
    [(if& cond true false)
     (type-check cond (boolean-ty))
     ;; TODO check that these match
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
          (raise-user-error 'type-check "Cannot apply function: Got ~s but expected ~s arguments"
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
                 (type-check arg (substitute body-substitution arg-type))))])]
       [t (raise-user-error 'type-check "Cannot apply non function type: ~a" t)])]
    [(varargs-app& op args)
     ;; TODO make this use the type information on the op instead
     (match (type-infer op)
       [(fun-ty stale-type-vars stale-arg-types stale-body-type)
        (match-define-values (type-vars (cons body-type arg-types))
          (freshen-types stale-type-vars (cons stale-body-type stale-arg-types)))
        (unless (equal? (length arg-types) 1)
          (raise-user-error 'type-check "Cannot varags apply function: It has ~s arguments."
                            (length arg-types)))
        (unless (array-ty? (first arg-types))
          (raise-user-error 'type-check "Cannot varags apply function: Its argument (~s) isn't an array."
                            (first arg-types)))
        (match-define (array-ty element-ty) (first arg-types))
        (cond
          [(unknown-ty? type)
           (cond
             [(empty? type-vars)
              (for ([arg (in-list args)])
                (type-check arg element-ty))
              (check body-type)]
             [else
               (check
                 (substitute
                   (unify-types type-vars (map (λ (t) (list element-ty t)) (map type-infer args)))
                   body-type))])]
          [else
           (define body-substitution (unify-types type-vars (list (list body-type type))))
           (if (member 'unused-type-var (hash-values body-substitution))
               (check
                 (substitute
                   (unify-types type-vars (map (λ (t) (list element-ty t)) (map type-infer args)))
                   body-type))
               (for ([arg (in-list args)])
                 (type-check arg (substitute body-substitution element-ty))))])])]
    [(let& name expr body)
     (let* ([expr-type (type-infer expr)]
            [type-env (binding-env-value-set env name expr-type)])
       ((type-check/env type-env) body type))]
    [(ann& pre-type expr)
     (define type ((parse-type/env (binding-env-types env)) pre-type))
     (type-check expr type)
     (check type)]
    [(lambda& (list (list args arg-pre-types) ...) pre-return-type (block& defs body))
     (define arg-types
       (for/list ([pre-type (in-list arg-pre-types)])
         ((parse-type/env (binding-env-types env)) pre-type)))
     (define return-type
       (and pre-return-type ((parse-type/env (binding-env-types env)) pre-return-type)))
     (let ([env (for/fold ([env env]) ([arg (in-list args)] [arg-type (in-list arg-types)])
                  (binding-env-value-set env arg arg-type))])
       (check
         (fun-ty
           empty
           arg-types
           (cond
             [return-type
               (begin
                 (type-check/block env defs body return-type)
                 return-type)]
             [(unknown-ty? type)
              (type-check/block env defs body (unknown-ty))]
             [else
              (match type
                [(fun-ty (list) arg-types body-type)
                 (begin
                   (type-check/block env defs body body-type)
                   body-type)]
                [(fun-ty _ arg-types body-type)
                 (raise-user-error 'typecheck "Expected a polymorphic function: got a lambda expression")]
                [_
                 (raise-user-error 'typecheck
                                   "Expected a non function: got a lambda expression")])]))))]
    [(case& expr clauses)
     (check-patterns-complete/not-useless env (map case-clause&-pattern clauses))


     (define expr-type (type-infer expr))

     (define types
       (for/set ([clause (in-list clauses)])
         (match clause
           [(case-clause& pattern (block& defs body))
            ;; The variables are already fresh
            (match-define (template-data type-vars var-types constraints pattern-type)
              ((pattern->template-data/env env) pattern))

            (define substitution
              (unify-types type-vars (cons (list pattern-type expr-type) constraints)))


            (define new-env
              (for/fold ([env env]) ([(field-var field-type) (in-hash var-types)])
                (binding-env-value-set env field-var (substitute substitution field-type))))

            (type-check/block new-env defs body type)])))


     (when (unknown-ty? type)
       (unless (= (set-count types) 1)
         (raise-user-error 'type-infer "Case clauses have conflicting result types"))
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
    [(byte-pattern& _) (template-data empty (hash) empty (int-ty))]
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

  (define (lookup-variants pattern-spec)
    (define variant-name (pattern-spec-variant-name pattern-spec))
    (define input-type (pattern-spec-input-type pattern-spec))
    (define ind-sigs
      (for*/set ([ind-sig (in-list (binding-env-inductive-signatures env))]
                 #:when
                   (and (equal? (inductive-signature-module-name ind-sig)
                                (data-ty-module-name input-type))
                        (equal? (inductive-signature-name ind-sig)
                                (data-ty-name input-type))))
        ind-sig))
    (unless (= (set-count ind-sigs) 1)
      (for ([ind-sig (in-list (binding-env-inductive-signatures env))])
        (eprintf "~a~n" ind-sig))
      (error 'lookup-other-variants "Bad variant ~s: ~s ~s" variant-name input-type
             (set->list ind-sigs)))
    (define abstract-values
      (for/hash ([variant-sig (in-list (inductive-signature-variants (set-first ind-sigs)))])
        (define name (variant-signature-name variant-sig))
        (values
          name
          (abstract-variant name (map (λ (_) (any-abstract-value))
                                      (variant-signature-types variant-sig))))))
    (values
      (hash-ref abstract-values variant-name)
      (list->set
        (hash-values (hash-remove abstract-values variant-name)))))

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
      [(byte-pattern& _) (values (set abstract-value) (set (some-abstract-value)))]
      [(or (variable-pattern& _) (ignore-pattern&))
       (values (set) (set abstract-value))]
      [(abstraction-pattern& pat-binding patterns)
       (define pat-spec (binding-env-pattern-ref env pat-binding))
       (define variant-name (pattern-spec-variant-name pat-spec))
       (match abstract-value
        [(any-abstract-value)
         ;; Compute all variants
         (define-values (refined-value other-variant-values)
           (lookup-variants pat-spec))
         (define-values (unmatched matched)
           (abstract-match pattern refined-value))
         (values
           (set-union unmatched other-variant-values)
           matched)]
        [(abstract-variant val-name fields)
         #:when (equal? variant-name val-name)
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
        (raise-user-error 'pattern-match "Unmatchable pattern: ~s" pattern))
      unmatched))
  (unless (set-empty? unmatched)
    (raise-user-error 'pattern-match "Unmatched values: ~s in ~s" unmatched patterns)))



