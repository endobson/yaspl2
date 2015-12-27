#lang racket/base

(require
  racket/list
  racket/match)

(struct module& (name imports exports definitions))

(struct definition& (args body))

(struct expression& ())
(struct byte& expression& (v))
(struct string& expression& (v))
(struct variable& expression& (v))
(struct if& expression& (cond true false))
(struct begin& expression& (exprs last-expr))
(struct app& expression& (op args))



(define (parse-module sexp)
  (match sexp
    [`(module ,(? symbol? name)
        (import . ,(app parse-imports imports))
        (export . ,(app parse-exports exports))
        . ,(app parse-definitions definitions))
     (module& name imports exports definitions)]))

(define (parse-imports imports)
  (unless (empty? imports)
    (error 'nyi "Imports are not yet implemented"))
  empty)

(define (parse-exports exports)
  (unless (empty? exports)
    (error 'nyi "Exports are not yet implemented"))
  empty)

(define (parse-definitions definitions)
  (unless (empty? definitions)
    (error 'nyi "Definitions are not yet implemented"))
  empty)


(define (parse-expression sexp)
  (define parse parse-expression)
  (match sexp
    [(? byte? num) (byte& num)]
    [(? string? str) (string& str)]
    [(? symbol? sym) (variable& sym)]
    [`(if ,cond ,true ,false)
     (if& (parse cond) (parse true) (parse false))]
    [(list 'begin exprs ... last-expr)
     (begin& (map parse exprs) (parse last-expr))]
    [(list op args ...)
     (app& (parse op) (map parse args))]))



(module+ test
  (define empty-module-src
    '(module empty
       (import)
       (export)))
  (define empty-module (parse-module empty-module-src)))
