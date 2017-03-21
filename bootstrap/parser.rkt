#lang racket/base

(require
  "parser-structs.rkt"
  racket/match
  racket/list
  racket/set
  (only-in racket/contract/base and/c))
(provide
  parse-module)


(define (parse-module sexps)
  (match sexps
    [`((module ,(? symbol? name)
         (import . ,(app parse-imports imports))
         (export . ,(app parse-exports exports))
         (types . ,(app parse-type-definitions types))
         . ,(app parse-definitions definitions)))
     (module& name imports exports types definitions)]
    [`(#:module ,(? symbol? name)
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
           (import& module-name function-name function-name))
         empty)]
      [`(,(? symbol? module-name)
         ,@`(
            ,@(list #:types ... (? list? types) ...)
            ,@(list #:values ... (? list? functions) ...)
            ,@(list #:patterns ... (? list? patterns) ...)))
       (define (parse-import-name import-name)
         (match import-name
           [(? symbol? name)
            (import& module-name name name)]
           [(list (? symbol? exported-name) (? symbol? local-name))
            (import& module-name exported-name local-name)]))
       (values
         (map parse-import-name (append* types))
         (map parse-import-name (append* functions))
         (map parse-import-name (append* patterns)))]))

  (let-values ([(types vals patterns) (parse-imports imports)])
    (imports& (append* types) (append* vals) (append* patterns))))

;; TODO support renaming
(define (parse-exports exports)
  (match exports
   [(list)
    (exports& empty empty empty) ]
   [(list
      #:types (list (? symbol? types) ...)
      #:values (list (? symbol? values) ...)
      #:patterns (list (? symbol? patterns) ...))
    (exports&
      (map export& types types)
      (map export& values values)
      (map export& patterns patterns))]))

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


(define (parse-definitions defs)
  (define (parse-definition sexp)
    (match sexp
      [`(define (,name (,(? symbol? args) : ,arg-types) ...) : ,return-type . ,body)
        (define type (fun-pre-type empty (map parse-pre-type arg-types) (parse-pre-type return-type)))
        (values name (definition& type args (parse-block body)))]
      [`(define (,(? symbol? type-vars) ...) (,name (,(? symbol? args) : ,arg-types) ...) :
                ,return-type . ,body)
        (define type (fun-pre-type type-vars (map parse-pre-type arg-types) (parse-pre-type return-type)))
        (values name (definition& type args (parse-block body)))]))

  (for/hash ([def (in-list defs)])
    (parse-definition def)))


(define (parse-block sexps)
  (define (recur sexps rev-defs)
    (match sexps
      [`(,expr)
       (block& (reverse rev-defs) (parse-expression expr))]
      [`((match-define ,pattern ,expr) . ,sexps)
        (recur sexps (cons (match-def& (parse-pattern pattern) (parse-expression expr)) rev-defs))]))
  (recur sexps empty))

(define (parse-expression sexp)
  (define parse parse-expression)
  (match sexp
    [(? exact-integer? num) (byte& num)]
    [(? (and/c bytes? immutable?) bytes) (bytes& bytes)]
    [(? boolean? bool) (boolean& bool)]
    [(? symbol? sym) (variable& sym)]
    [`(if ,cond ,true ,false)
     (if& (parse cond) (parse true) (parse false))]
    [(list 'begin first-expr exprs ...)
     (begin& (parse first-expr) (map parse exprs))]
    [(list 'varargs first-expr exprs ...)
     (varargs-app& (parse first-expr) (map parse exprs))]
    [`(let ([,(? symbol? name) ,expr]) ,body)
     (let& name (parse expr) (parse body))]
    [`(case ,expr . ,(list (cons patterns bodies) ...))
     (case& (parse expr)
            (for/list ([pattern (in-list patterns)]
                       [body (in-list bodies)])
              (case-clause&
                (parse-pattern pattern)
                (parse-block body))))]
    [`(lambda ([,(? symbol? args) : ,arg-types] ...) ,body)
     (lambda& (map list args (map parse-pre-type arg-types)) #f (parse body))]
    [`(lambda ([,(? symbol? args) : ,arg-types] ...) : ,return-type ,body)
     (lambda& (map list args (map parse-pre-type arg-types)) (parse-pre-type return-type) (parse body))]

    [(list op args ...)
     (app& (parse op) (map parse args))]))

(define (parse-pattern sexp)
  (match sexp
    [(? (and/c bytes? immutable?) v) (bytes-pattern& v)]
    ['_ (ignore-pattern&)]
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

