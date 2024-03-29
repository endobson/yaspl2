#lang racket/base

(require
  "parser-structs.rkt"
  "utils.rkt"
  racket/match
  racket/list
  racket/set
  racket/string
  (only-in racket/contract/base and/c))
(provide
  parse-module)


(define (parse-module sexps)
  (match sexps
    [`(#:module (,(? symbol? name) ...)
       #:import ,(app parse-imports imports)
       (export . ,(app parse-exports exports))
       (types . ,(app parse-type-definitions types))
       . ,(app parse-top-level-definitions fun-definitions static-definitions))
     (module& (module-name& name) imports exports types fun-definitions static-definitions)]))

(define (parse-imports imports)
  (define (recur imports)
    (match imports
      [(list) empty]
      [(cons (list (? symbol? module-name) ...) rest)
       (cons (full-imports& (module-name& module-name)) (recur rest))]
      [(cons (list (list (? symbol? module-name) ...) forms ...) rest)
       (cons (parse-import-section (module-name& module-name) forms) (recur rest))]))
  (define (parse-import-section module-name forms)
    (define (parse-import-elem import-elem)
      (match import-elem
        [(? symbol? name)
         (import& name name)]
        [(list (? symbol? exported-name) (? symbol? local-name))
         (import& exported-name local-name)]))
    (define (type-point forms)
      (match forms
        [(cons (list #:types (? import-elem? import-elems) ...) forms)
         (cons (map parse-import-elem import-elems)
               (value-point forms))]
        [forms
         (cons empty (value-point forms))]))
    (define (value-point forms)
      (match forms
        [(cons (list #:values (? import-elem? import-elems) ...) forms)
         (cons (map parse-import-elem import-elems)
               (pattern-point forms))]
        [forms
         (cons empty (pattern-point forms))]))
    (define (pattern-point forms)
      (match forms
        [(cons (list #:patterns (? import-elem? import-elems) ...) forms)
         (cons (map parse-import-elem import-elems)
               (statics-point forms))]
        [forms
         (cons empty (statics-point forms))]))
    (define (statics-point forms)
      (match forms
        [(cons (list #:statics (? import-elem? import-elems) ...) forms)
         (cons (map parse-import-elem import-elems)
               (empty-point forms))]
        [forms
         (cons empty (empty-point forms))]))
    (define (empty-point forms)
      (match forms
        [(list)
         (list)]))
    (match (type-point forms)
      [(list types values patterns statics)
       (partial-imports& module-name types values patterns statics)]))
  (define (import-elem? x)
    (match x
      [(? symbol?) #t]
      [(list (? symbol?) (? symbol?)) #t]
      [_ #f]))

  (recur imports))

;; TODO support renaming
(define (parse-exports exports)
  (match exports
   [(list)
    (exports& empty empty empty empty) ]
   [(list
      (list #:types (? symbol? types*) ...) ...
      (list #:values (? symbol? values*) ...) ...
      (list #:patterns (? symbol? patterns*) ...) ...
      (list #:statics (? symbol? statics*) ...) ...)
    (define types (append* types*))
    (define values (append* values*))
    (define patterns (append* patterns*))
    (define statics (append* statics*))
    (exports&
      (map export& types types)
      (map export& values values)
      (map export& patterns patterns)
      (map export& statics statics))]))

(define (parse-type-definitions types)
  (define (parse-variant variant)
    (match variant
      [(list variant-name [list field-name ': (app parse-pre-type field-type)] ...)
       (variant& variant-name (map variant-field& field-name field-type))]))
  (define (parse-type-definition type)
    (match type
      [`(define-type ,(? symbol? type-name) . ,(list (app parse-variant variants) ...))
       (define-type& type-name #f variants)]
      [`(define-type ,(list (? symbol? type-name) (? symbol? type-variables) ..1)
                     . ,(list (app parse-variant variants) ...))
       (define-type& type-name type-variables variants)]))
  (map parse-type-definition types))


(define (parse-top-level-definitions defs)
  (define fun-defs (make-hash))
  (define static-defs (make-hash))


  (define (parse-top-level-definition sexp)
    (match sexp
      [`(define (,name (,(? symbol? args) : ,arg-types) ...) : ,return-type . ,body)
        (define type (fun-pre-type empty (map parse-pre-type arg-types) (parse-pre-type return-type)))
        (hash-set! fun-defs name (definition& type args (parse-block body)))]
      [`(define (,(? symbol? type-vars) ...) (,name (,(? symbol? args) : ,arg-types) ...) :
                ,return-type . ,body)
        (define type (fun-pre-type type-vars (map parse-pre-type arg-types) (parse-pre-type return-type)))
        (hash-set! fun-defs name (definition& type args (parse-block body)))]
      [`(define/varargs ,name : (,arg-type ,return-type) ,(? symbol? cons-name) ,(? symbol? empty-name))
        (hash-set!
          static-defs
          name
          (varargs-definition& empty (parse-pre-type arg-type) (parse-pre-type return-type)
                               cons-name empty-name))]
      [`(define/varargs (,(? symbol? type-vars) ...) ,name :
                        (,arg-type ,return-type) ,(? symbol? cons-name) ,(? symbol? empty-name))
        (hash-set!
          static-defs
          name
          (varargs-definition& type-vars (parse-pre-type arg-type) (parse-pre-type return-type)
                               cons-name empty-name))]))
  (for-each parse-top-level-definition defs)

  (values
    (hash-copy/immutable fun-defs)
    (hash-copy/immutable static-defs)))


(define (parse-block sexps)
  (define (recur sexps rev-defs)
    (match sexps
      [`(,expr)
       (block& (reverse rev-defs) (parse-expression expr))]
      [`((match-define ,pattern ,expr) . ,sexps)
        (recur sexps (cons (match-def& (parse-pattern pattern) #f (parse-expression expr)) rev-defs))]
      [`((match-define ,pattern : ,type ,expr) . ,sexps)
        (recur sexps (cons (match-def& (parse-pattern pattern) (parse-pre-type type) (parse-expression expr))
                           rev-defs))]))
  (recur sexps empty))

(define (parse-expression sexp)
  (define parse parse-expression)
  (match sexp
    [(? exact-integer? num) (byte& num)]
    [(? bytes? bytes) (bytes& (bytes->immutable-bytes bytes))]
    [(? boolean? bool) (boolean& bool)]
    [(? symbol? sym) (variable& sym)]
    [`(if ,cond ,true ,false)
     (if& (parse cond) (parse true) (parse false))]
    [`(cond [,tests . ,bodies] ... [else . ,final-body])
      (cond&
        (for/list ([test (in-list tests)]
                   [body (in-list bodies)])
          (cond-clause& (parse-expression test) (parse-block body)))
        (parse-block final-body))]
    [(list 'begin first-expr exprs ...)
     (begin& (parse first-expr) (map parse exprs))]
    [(list 'varargs (? symbol? name) exprs ...)
     (varargs2-app& name (map parse exprs))]
    [`(let ([,(? symbol? name) ,expr]) . ,body)
     (let& name (parse expr) (parse-block body))]
    [`(case ,expr . ,(list (cons patterns bodies) ...))
     (case& (parse expr)
            (for/list ([pattern (in-list patterns)]
                       [body (in-list bodies)])
              (case-clause&
                (parse-pattern pattern)
                (parse-block body))))]
    [`(ann ,type ,expr)
     (ann& (parse-pre-type type) (parse-expression expr))]
    [`(lambda ([,(? symbol? args) : ,arg-types] ...) : ,return-type . ,body)
     (lambda& (map list args (map parse-pre-type arg-types)) (parse-pre-type return-type) (parse-block body))]
    [`(lambda ([,(? symbol? args) : ,arg-types] ...) . ,body)
     (lambda& (map list args (map parse-pre-type arg-types)) #f (parse-block body))]

    [(list op args ...)
     (app& (parse op) (map parse args))]))

(define (parse-pattern sexp)
  (match sexp
    [(? bytes? bytes) (bytes-pattern& (bytes->immutable-bytes bytes))]
    [(? (lambda (s) (and (symbol? s) (string-prefix? (symbol->string s) "_"))))
     (ignore-pattern&)]
    [(? symbol? v) (variable-pattern& v)]
    [(? symbol? v) (variable-pattern& v)]
    [(? integer? v) (byte-pattern& v)]
    [(list (? symbol? name) patterns ...)
     (abstraction-pattern& name (map parse-pattern patterns))]))

(define (parse-pre-type sexp)
  (match sexp
    [(? symbol?) (var-pre-type sexp)]
    [(list arg-types ... '-> result-type)
     (fun-pre-type empty (map parse-pre-type arg-types) (parse-pre-type result-type))]
    [(list (? symbol? type-constructor) arg-types ...)
     (type-app-pre-type type-constructor (map parse-pre-type arg-types))]))

