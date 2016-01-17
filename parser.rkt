#lang racket/base

(require
  "parser-structs.rkt"
  racket/match
  racket/list
  (only-in racket/contract/base and/c))
(provide
  parse-module)


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
        (define type (fun-pre-type empty (map parse-pre-type arg-types) (parse-pre-type return-type)))
        (values name (definition& type args (parse-expression body)))]
      [`(define (,(? symbol? type-vars) ...) (,name (,(? symbol? args) : ,arg-types) ...) : ,return-type ,body)
        (define type (fun-pre-type type-vars (map parse-pre-type arg-types) (parse-pre-type return-type)))
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
     (fun-pre-type empty (map parse-pre-type arg-types) (map parse-pre-type result-type))]
    [(list (? symbol? type-constructor) arg-types ...)
     (type-app-pre-type type-constructor (map parse-pre-type arg-types))]))

