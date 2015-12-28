#lang racket/base

(require
  racket/list
  racket/match)

(struct module& (name imports exports definitions))

(struct export& (name))
(struct definition& (args body))

(struct expression& ())
(struct byte& expression& (v))
(struct boolean& expression& (v))
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
  (match exports
   [(list (? symbol? exports) ...)
    (map export& exports)]))

(define (parse-definitions defs)
  (define (parse-definition sexp)
    (match sexp
      [`(define (,name . ,(list (? symbol? args) ...)) ,body)
        (values name (definition& args (parse-expression body)))]))
  (for/hash ([def (in-list defs)])
    (parse-definition def)))


(define (parse-expression sexp)
  (define parse parse-expression)
  (match sexp
    [(? byte? num) (byte& num)]
    [(? string? str) (string& str)]
    [(? boolean? bool) (boolean& bool)]
    [(? symbol? sym) (variable& sym)]
    [`(if ,cond ,true ,false)
     (if& (parse cond) (parse true) (parse false))]
    [(list 'begin exprs ... last-expr)
     (begin& (map parse exprs) (parse last-expr))]
    [(list op args ...)
     (app& (parse op) (map parse args))]))


;;;;

(struct value ())
(struct function-val value (args env body))
(struct byte-val value (v))
(struct boolean-val value (v))
(struct string-val value (v))

(struct halt-k ())
(struct apply-k (vals args env cont))
(struct if-k (true false env cont))

(struct full-name (module-name main-name) #:transparent)

;; Ties the not of recursive global functions
(define (make-global-env modules)
  (define global-env (make-hash))
  (for ([module modules])
    (define local-env (make-hash))

    (for ([(name def) (in-hash (module&-definitions module))])
      (define val
        (match def
          [(definition& args body)
           (function-val args local-env body)]))
      (hash-set! local-env name val))
    (for ([export (in-list (module&-exports module))])
      (define name (export&-name export))
      (hash-set! global-env (full-name (module&-name module) name)
                 (hash-ref local-env name))))
  global-env)


(define (run-program modules module-name main-name)
  (define env (make-global-env modules))
  (define full-main-name (full-name module-name main-name))
  (define main-fun (hash-ref env full-main-name #f))
  (unless main-fun
    (error 'run-program "Main function is not exported: ~s in ~s" full-main-name))
  (unless (function-val? main-fun)
    (error 'run-program "Main function is not a function value: ~s" main-fun))
  (unless (zero? (length (function-val-args main-fun)))
    (error 'run-program "Main function does not have correct arity: ~s" main-fun))

  (byte-val-v
    (run-machine
      (call-function main-fun empty (halt-k)))))

(struct eval-machine-state (expr env cont))
(struct apply-machine-state (vals exprs env cont))
(struct cont-machine-state (val cont))
(struct error-machine-state (info))



(define (call-function fun args cont)
  (match fun
    [(function-val arg-names env body)
     (define new-env
       (for/fold ([env (hash-copy/immutable env)])
                 ([v (in-list args)] [name (in-list arg-names)])
         (hash-set env name v)))
     (eval-machine-state body new-env cont)]))

(define (hash-copy/immutable env)
  (make-immutable-hash (hash->list env)))


(define (run-machine machine)
  (match machine
    [(error-machine-state info)
     (error 'run-machine "Error running the machine: %s" info)]
    [(eval-machine-state expr env cont)
     (match expr
       [(byte& v)
        (run-machine (cont-machine-state (byte-val v) cont))]
       [(boolean& v)
        (run-machine (cont-machine-state (boolean-val v) cont))]
       [(variable& v)
        (run-machine (cont-machine-state (hash-ref env v) cont))]
       [(app& op vs)
        (run-machine (apply-machine-state empty (cons op vs) env cont))]
       [(if& cond true false)
        (run-machine (eval-machine-state cond env (if-k true false env cont)))])]
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
       [(apply-k vals args env cont)
        (run-machine (apply-machine-state (cons val vals) args env cont))]
       [(if-k true false env cont)
        (define expr (if (boolean-val-v val) true false))
        (run-machine (eval-machine-state expr env cont))])]))



(module+ test
  (require racket/set)
  (require rackunit)

  (define modules (mutable-set))
  (define (add-module! module)
    (set-add! modules (parse-module module)))

  (define (yaspl-test module-name main-name exit-code)
    (check-equal? (run-program modules module-name main-name) exit-code))


  (add-module!
    '(module empty
       (import)
       (export)))

  (add-module!
    '(module exit-code
       (import)
       (export main)
       (define (main)
         1)))

  (add-module!
    '(module exit-code2
       (import)
       (export main helper)
       (define (main)
         (helper))
       (define (helper)
         2)))

  (add-module!
    '(module exit-code3
       (import)
       (export main helper)
       (define (main)
         (helper 3))
       (define (helper x)
         x)))

  (add-module!
    '(module exit-code4
       (import)
       (export main)
       (define (main)
         (if #t 4 5))))



  (yaspl-test 'exit-code 'main 1)
  (yaspl-test 'exit-code2 'main 2)
  (yaspl-test 'exit-code3 'main 3)
  (yaspl-test 'exit-code4 'main 4)
  )
