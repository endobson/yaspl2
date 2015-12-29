#lang racket/base

(require
  racket/list
  racket/set
  racket/match)

(struct module& (name imports exports definitions))

(struct export& (name))
(struct import& (module-name name))
(struct definition& (args body))

(struct expression& ())
(struct byte& expression& (v))
(struct boolean& expression& (v))
(struct string& expression& (v))
(struct variable& expression& (v))
(struct if& expression& (cond true false))
(struct begin& expression& (first-expr exprs))
(struct app& expression& (op args))


(define (parse-module sexp)
  (match sexp
    [`(module ,(? symbol? name)
        (import . ,(app parse-imports imports))
        (export . ,(app parse-exports exports))
        . ,(app parse-definitions definitions))
     (module& name imports exports definitions)]))

(define (parse-imports imports)
  (match imports
    [(list (list (? symbol? module-names) (? symbol? function-namess) ...) ...)
     (for/list ([module-name (in-list module-names)]
                [function-names (in-list function-namess)]
                #:when #t
                [function-name (in-list function-names)])
       (import& module-name function-name))]))

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
    [(list 'begin first-expr exprs ...)
     (begin& (parse first-expr) (map parse exprs))]
    [(list op args ...)
     (app& (parse op) (map parse args))]))


;;;;

(struct value ())
(struct function-val value (args env body))
(struct void-val value ())
(struct byte-val value (v))
(struct boolean-val value (v))
(struct bytes-val value (v))

(struct prim-port-val value (port))
(struct prim-function-val value (name))

(struct halt-k ())
(struct apply-k (vals args env cont))
(struct if-k (true false env cont))
(struct ignore-k (expr env cont))

(struct full-name (module-name main-name) #:transparent)


(define (topo-sort modules)
  (define module-hash
    (for/hash ([module (in-list modules)])
      (values (module&-name module) module)))

  (define edges
    (hash-copy
      (for/hash ([module (in-list modules)])
        (define imports (list->mutable-set (map import&-module-name (module&-imports module))))
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
    (error 'topo-sort "Something went wrong"))
  (map (λ (name) (hash-ref module-hash name)) (reverse order)))





;; Ties the knot of recursive global functions
(define (make-global-env modules)
  (define (make-primitive-environment)
    (define prims '(+ write-byte make-bytes read-bytes bytes-ref))
    (hash-copy
      (for/hash ([prim (in-list prims)])
        (values (full-name 'prim prim) (prim-function-val prim)))))


  (define global-env (make-primitive-environment))


  (for ([module (topo-sort (set->list modules))])
    (define local-env (make-hash))

    (for ([import (in-list (module&-imports module))])
      (hash-set! local-env (import&-name import)
                 (hash-ref global-env
                           (full-name (import&-module-name import) (import&-name import)))))

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

(struct program-result (exit-code stdout stderr))

(define (run-program modules module-name main-name #:stdin stdin-bytes)
  (define env (make-global-env modules))
  (define full-main-name (full-name module-name main-name))
  (define main-fun (hash-ref env full-main-name #f))
  (unless main-fun
    (error 'run-program "Main function is not exported: ~s in ~s" full-main-name))
  (unless (function-val? main-fun)
    (error 'run-program "Main function is not a function value: ~s" main-fun))
  (unless (equal? (length (function-val-args main-fun)) 3)
    (error 'run-program "Main function does not have correct arity: ~s" main-fun))

  (define stdout (open-output-bytes 'stdout))
  (define stderr (open-output-bytes 'stderr))
  (define stdin (open-input-bytes stdin-bytes 'stderr))
  (define args (list (prim-port-val stdin) (prim-port-val stdout) (prim-port-val stderr)))

  (program-result
    (byte-val-v
      (run-machine
        (call-function main-fun args (halt-k))))
    (get-output-bytes stdout)
    (get-output-bytes stderr)))

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
     (eval-machine-state body new-env cont)]
    [(prim-function-val name)
     (case name
       [(+)
        (match args
          [(list (byte-val x) (byte-val y))
           (cont-machine-state (byte-val (+ x y)) cont)])]
       [(write-byte)
        (match args
          [(list (byte-val x) (prim-port-val p))
           (write-byte x p)
           (cont-machine-state (void-val) cont)])]
       [(make-bytes)
        (match args
          [(list (byte-val size))
           (cont-machine-state (bytes-val (make-bytes size)) cont)])]
       [(read-bytes)
        (match args
          [(list (bytes-val b) (prim-port-val p) (byte-val offset) (byte-val amount))
           (define amount-read (read-bytes! b p offset amount))
           (cont-machine-state (byte-val amount-read) cont)])]
       [(bytes-ref)
        (match args
          [(list (bytes-val b) (byte-val index))
           (cont-machine-state (byte-val (bytes-ref b index)) cont)])])]))

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
        (run-machine (eval-machine-state cond env (if-k true false env cont)))]
       [(begin& first-expr exprs)
        (run-machine
          (eval-machine-state first-expr env
            (for/fold ([cont cont]) ([expr (in-list (reverse exprs))])
              (ignore-k expr env cont))))])]
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
        (run-machine (eval-machine-state expr env cont))])]))



(module+ test
  (require racket/set)
  (require rackunit)

  (define modules (mutable-set))
  (define (add-module! module)
    (set-add! modules (parse-module module)))

  (define (yaspl-test module-name main-name 
                      #:exit-code [exit-code 0]
                      #:stdin [stdin #""]
                      #:stdout [stdout #""]
                      #:stderr [stderr #""])
    (test-case (format "~a/~a" module-name main-name)
      (define result (run-program modules module-name main-name #:stdin stdin))
      (check-equal? (program-result-exit-code result) exit-code)
      (check-equal? (program-result-stdout result) stdout)
      (check-equal? (program-result-stderr result) stderr)))


  (add-module!
    '(module empty
       (import)
       (export)))

  (add-module!
    '(module exit-code
       (import)
       (export main)
       (define (main stdin stdout stderr)
         1)))

  (add-module!
    '(module exit-code2
       (import)
       (export main helper)
       (define (main stdin stdout stderr)
         (helper))
       (define (helper)
         2)))

  (add-module!
    '(module exit-code3
       (import)
       (export main helper)
       (define (main stdin stdout stderr)
         (helper 3))
       (define (helper x)
         x)))

  (add-module!
    '(module exit-code4
       (import)
       (export main)
       (define (main stdin stdout stderr)
         (if #t 4 5))))

  (add-module!
    '(module exit-code5
       (import (prim +))
       (export main)
       (define (main stdin stdout stderr)
         (+ 2 3))))


  (add-module!
    '(module exit-code6
       (import (exit-code6-helper times2))
       (export main)
       (define (main stdin stdout stderr)
         (times2 3))))

  (add-module!
    '(module exit-code6-helper
       (import (prim +))
       (export times2)
       (define (times2 x)
         (+ x x))))


  (add-module!
    '(module stdout1
       (import (prim write-byte))
       (export main)
       (define (main stdin stdout stderr)
         (begin
           (write-byte 65 stdout)
           (write-byte 97 stdout)
           0))))

  (add-module!
    '(module stdin1
       (import (prim make-bytes read-bytes bytes-ref))
       (export main)
       (define (helper in bytes)
         (begin
           (read-bytes bytes in 0 5)
           (bytes-ref bytes 0)))

       (define (main stdin stdout stderr)
         (helper stdin (make-bytes 5)))))


  (yaspl-test 'exit-code 'main #:exit-code 1)
  (yaspl-test 'exit-code2 'main #:exit-code 2)
  (yaspl-test 'exit-code3 'main #:exit-code 3)
  (yaspl-test 'exit-code4 'main #:exit-code 4)
  (yaspl-test 'exit-code5 'main #:exit-code 5)
  (yaspl-test 'exit-code6 'main #:exit-code 6)

  (yaspl-test 'stdout1 'main #:stdout #"Aa")

  (yaspl-test 'stdin1 'main #:stdin #"A" #:exit-code 65)

  )
