#lang racket/base

(require
  racket/list
  racket/set
  racket/match)

(struct module& (name imports exports types definitions))

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
(struct let& expression& (name expr body))
(struct case& expression& (expr clauses))
(struct case-clause& (variant-name field-variables expr))

(struct define-type& (type-name type-variables variants))
(struct variant& (name fields))
(struct variant-field& (name type))

(define (parse-module sexp)
  (match sexp
    [`(module ,(? symbol? name)
        (import . ,(app parse-imports imports))
        (export . ,(app parse-exports exports))
        (types . ,(app parse-types types))
        . ,(app parse-definitions definitions))
     (module& name imports exports types definitions)]))

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

(define (parse-types types)
  (define (parse-variant variant)
    (match variant
      [(list variant-name [list field-name field-type] ...)
       (variant& variant-name (map variant-field& field-name field-type))]))
  (define (parse-type type)
    (match type
      [`(define-type ,(? symbol? type-name) . ,(list (app parse-variant variants) ...))
       (define-type& type-name #f variants)]
      [`(define-type ,(list (? symbol? type-name) (? symbol? type-variables) ..1)
                     . ,(list (app parse-variant variants) ...))
       (define-type& type-name type-variables variants)]))
  (map parse-type types))


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
    [`(let ([,(? symbol? name) ,expr]) ,body)
     (let& name (parse expr) (parse body))]
    [`(case ,expr . ,(list (list (list (? symbol? variant-names) (? symbol? field-namess) ...) bodies) ...))
     (case& (parse expr) (map case-clause& variant-names field-namess (map parse bodies)))]
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

(struct variant-val (variant-name fields))
(struct variant-constructor-val (variant-name fields))
(struct field-accessor-val value (variant-name field-name))

(struct halt-k ())
(struct apply-k (vals args env cont))
(struct if-k (true false env cont))
(struct ignore-k (expr env cont))
(struct bind-k (name expr env cont))
(struct case-k (clauses env cont))

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
    (define prims '(+ - * = or void write-byte make-bytes read-bytes bytes-ref bytes-length bytes-set!))
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
    (error 'run-program "Main function is not exported: ~s in ~s" full-main-name module-name))
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
    [(variant-constructor-val variant-name fields)
     (unless (= (length args) (length fields))
       (error 'variant-constructor "Wrong number of arguments for ~a: Expected ~a, got ~a"
              variant-name (length fields) (length args)))
     (cont-machine-state (variant-val variant-name args) cont)]
    [(field-accessor-val variant-name index)
     (match args
       [(list (variant-val (== variant-name equal?) fields))
        (cont-machine-state (list-ref fields index) cont)])]
    [(prim-function-val name)
     (case name
       [(or)
        (match args
          [(list (boolean-val x) (boolean-val y))
           (cont-machine-state (boolean-val (or x y)) cont)])]

       [(+)
        (match args
          [(list (byte-val x) (byte-val y))
           (cont-machine-state (byte-val (+ x y)) cont)])]
       [(-)
        (match args
          [(list (byte-val x) (byte-val y))
           (cont-machine-state (byte-val (- x y)) cont)])]
       [(*)
        (match args
          [(list (byte-val x) (byte-val y))
           (cont-machine-state (byte-val (* x y)) cont)])]
       [(=)
        (match args
          [(list (byte-val x) (byte-val y))
           (cont-machine-state (boolean-val (= x y)) cont)])]
       [(void)
        (match args
          [(list)
           (cont-machine-state (void-val) cont)])]
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
          [(list (bytes-val b) (prim-port-val p) (byte-val start-pos) (byte-val end-pos))
           (define amount-read (read-bytes! b p start-pos end-pos))
           (cont-machine-state (byte-val (if (eof-object? amount-read) 0 amount-read)) cont)])]
       [(bytes-ref)
        (match args
          [(list (bytes-val b) (byte-val index))
           (cont-machine-state (byte-val (bytes-ref b index)) cont)])]
       [(bytes-set!)
        (match args
          [(list (bytes-val b) (byte-val index) (byte-val v))
           (bytes-set! b index v)
           (cont-machine-state (void) cont)])]
       [(bytes-length)
        (match args
          [(list (bytes-val b))
           (cont-machine-state (byte-val (bytes-length b)) cont)])])]))

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
       (export)
       (types)))

  (add-module!
    '(module exit-code
       (import)
       (export main)
       (types)
       (define (main stdin stdout stderr)
         1)))

  (add-module!
    '(module exit-code2
       (import)
       (export main helper)
       (types)
       (define (main stdin stdout stderr)
         (helper))
       (define (helper)
         2)))

  (add-module!
    '(module exit-code3
       (import)
       (export main helper)
       (types)
       (define (main stdin stdout stderr)
         (helper 3))
       (define (helper x)
         x)))

  (add-module!
    '(module exit-code4
       (import)
       (export main)
       (types)
       (define (main stdin stdout stderr)
         (if #t 4 5))))

  (add-module!
    '(module exit-code5
       (import (prim +))
       (export main)
       (types)
       (define (main stdin stdout stderr)
         (+ 2 3))))


  (add-module!
    '(module exit-code6
       (import (exit-code6-helper times2))
       (export main)
       (types)
       (define (main stdin stdout stderr)
         (times2 3))))

  (add-module!
    '(module exit-code6-helper
       (import (prim +))
       (export times2)
       (types)
       (define (times2 x)
         (+ x x))))


  (add-module!
    '(module stdout1
       (import (prim write-byte))
       (export main)
       (types)
       (define (main stdin stdout stderr)
         (begin
           (write-byte 65 stdout)
           (write-byte 97 stdout)
           0))))

  (add-module!
    '(module stdin1
       (import (prim make-bytes read-bytes bytes-ref))
       (export main)
       (types)

       (define (main stdin stdout stderr)
         (let ([bytes (make-bytes 5)])
           (begin
             (read-bytes bytes stdin 0 5)
             (bytes-ref bytes 0))))))

  (add-module!
    '(module bytes-output
       (import (prim write-byte = bytes-ref + - void))
       (export write-bytes)
       (types)

       (define (write-bytes bytes out offset amount)
         (if (= amount 0)
             (void)
             (begin
               (write-byte (bytes-ref bytes offset) out)
               (write-bytes bytes out (+ offset 1) (- amount 1)))))))



  (add-module!
    '(module echo1
       (import (prim make-bytes write-byte read-bytes bytes-ref + - = void)
               (bytes-output write-bytes))
       (export main)
       (types)

       (define (loop in out size)
         (let ([bytes (make-bytes size)])
           (let ([amount-read (read-bytes bytes in 0 size)])
             (if (= amount-read 0)
                 (void)
                 (begin
                   (write-bytes bytes out 0 amount-read)
                   (loop in out (if (= amount-read size) (+ size size) size)))))))


       (define (main stdin stdout stderr)
         (begin
           (loop stdin stdout 1)
           0))))

  (add-module!
    '(module echo2
       (import (prim bytes-length)
               (io read-all-bytes)
               (bytes-output write-bytes))
       (export main)
       (types)

       (define (loop in out size)
         (let ([bytes (make-bytes size)])
           (let ([amount-read (read-bytes bytes in 0 size)])
             (if (= amount-read 0)
                 (void)
                 (begin
                   (write-bytes bytes out 0 amount-read)
                   (loop in out (+ size size)))))))


       (define (main stdin stdout stderr)
         (begin
           (let ([bytes (read-all-bytes stdin)])
             (write-bytes bytes stdout 0 (bytes-length bytes)))
           0))))


  (add-module!
    '(module sum-tree
        (import (prim +))
        (export main)
        (types
          (define-type Tree
            (node [v Byte] [left Tree] [right Tree])
            (leaf)))

        (define (sum-tree t)
          (case t
            [(node v left right) (+ v (+ (sum-tree left) (sum-tree right)))]
            [(leaf) 0]))

        (define (main stdin stdout stderr)
          (sum-tree (node 4
                          (node 3 (node 1 (leaf) (node 2 (leaf) (leaf))) (leaf))
                          (node 5 (leaf) (leaf)))))))

  (add-module!
    '(module bytes
        (import (prim + - = bytes-set! bytes-ref void))
        (export bytes-copy)
        (types)

        (define (bytes-copy src s-off s-len dest d-off)
          (if (= s-len 0)
              (void)
              (begin
                (bytes-set! dest d-off (bytes-ref src s-off))
                (bytes-copy src (+ s-off 1) (- s-len 1) dest (+ d-off 1)))))))


  (add-module!
    '(module io
        (import (prim make-bytes read-bytes + * - bytes-length =)
                (bytes bytes-copy))
        (export read-all-bytes)
        (types)

        (define (read-all-bytes in)
          (read-all-bytes-loop in (make-bytes 1) 0))

        (define (read-all-bytes-loop in buf cur-size)
          (let ([amount-read (read-bytes buf in cur-size (bytes-length buf))])
            (if (= amount-read 0)
                (let ([trim-bytes (make-bytes cur-size)])
                   (begin
                     (bytes-copy buf 0 cur-size trim-bytes 0)
                     trim-bytes))
                (let ([new-size (+ amount-read cur-size)])
                  (if (= new-size (bytes-length buf))
                      (let ([new-buf (make-bytes (* 2 (bytes-length buf)))])
                        (begin
                          (bytes-copy buf 0 new-size new-buf 0)
                          (read-all-bytes-loop in new-buf new-size)))
                      (read-all-bytes-loop in buf new-size))))))))

  (add-module!
    '(module list
        (import)
        (export cons empty cons-head cons-tail reverse)
        (types
          (define-type (List a)
            (cons [head a] [tail (List a)])
            (empty)))

        (define (reverse list)
          (reverse-helper list empty))
        (define (reverse-helper l1 l2)
          (case l1
            [(empty) l2]
            [(cons hd tl) (reverse-helper tl (cons hd l2))]))))

  (add-module!
    '(module either
        (import)
        (export left right)
        (types
          (define-type (Either a b)
            (left [v a])
            (right [v a])))))

  (add-module!
    '(module maybe
        (import)
        (export just nothing)
        (types
          (define-type (Maybe a)
            (just [v a])
            (nothing)))))


  (add-module!
    '(module sexp-parser
       (import (lexer make-lexer run-lexer lex-result-v lex-result-next)
               (io read-all-bytes)
               (prim void)
               (either left right)
               (list cons empty reverse))
       (export main)
       (types
         (define-type Sexp
            (node [list List]))

         (define-type SexpResult
            (sexp-result [v Sexp] [lexer Lexer])
            (sexp-result-error)))



       (define (parse-sexp bytes)
         (let ([lexer (make-lexer bytes)])
           (let ([val (loop lexer)])
             (case val
               [(sexp-result v lexer)
                 (case (run-lexer lexer)
                   [(lex-result v lexer) (left (void))]
                   [(bad-input) (left (void))]
                   [(end-of-input) (right v)])]
               [(sexp-result-error) (left (void))]))))

       (define (loop lexer)
         (let ([val (run-lexer lexer)])
           (case val
             [(end-of-input) (sexp-result-error)]
             [(bad-input) (sexp-result-error)]
             [(lex-result v lexer)
               (case v
                 [(left-paren) (node-loop (empty) lexer)]
                 [(right-paren) (sexp-result-error)])])))

       (define (node-loop vals lexer)
         (let ([val (run-lexer lexer)])
           (case val
             [(end-of-input) (sexp-result-error)]
             [(bad-input) (sexp-result-error)]
             [(lex-result v lexer)
               (case (lex-result-v val)
                 [(left-paren)
                   (case (node-loop (empty) lexer)
                     [(sexp-result v lexer)
                      (node-loop (cons v vals) lexer)]
                     [(sexp-result-error) (sexp-result-error)])]
                 [(right-paren) (sexp-result (node (reverse vals)) lexer)])])))

       (define (main stdin stderr stdout)
         (let ([result (parse-sexp (read-all-bytes stdin))])
           (case result
             [(right v) 0]
             [(left v) 1])))))




  (add-module!
    '(module lexer
        (import (prim + = make-bytes read-bytes bytes-length bytes-ref or)
                (io read-all-bytes))
        (export main make-lexer run-lexer lex-result-v lex-result-next )
        (types
          (define-type Lexer
            (lexer [input Bytes] [pos Byte]))

          (define-type Lexeme
            (left-paren)
            (right-paren)
            ;; TODO support more cases
            #; #;
            (number [v Bytes])
            (symbol [v Bytes]))

          (define-type Result
            (lex-result [v lexeme] [next Lexer])
            (end-of-input)
            (bad-input)))

        #;
        (define (digit? v)
          (and (<= 48 v) (< v 58)))

        #;
        (define (digit->number v)
          (- v 48))

        ;; Space or Newline
        (define (whitespace? v)
          (or (= v 32) (= v 10)))

        (define (make-lexer bytes)
          (lexer bytes 0))

        (define (run-lexer lexer)
          (run (lexer-input lexer) (lexer-pos lexer)))


        (define (run bytes pos)
          (if (= pos (bytes-length bytes))
              (end-of-input)
              (let ([byte (bytes-ref bytes pos)])
                (if (= byte 40)
                    (lex-result (left-paren) (lexer bytes (+ pos 1)))
                    (if (= byte 41)
                        (lex-result (right-paren) (lexer bytes (+ pos 1)))
                        (if (whitespace? byte)
                            (run bytes (+ 1 pos))
                            (bad-input)))))))

        (define (loop lexer)
          (case (run-lexer lexer)
            [(lex-result v lexer) (loop lexer)]
            [(end-of-input) 0]
            [(bad-input) 1]))

        (define (main stdin stdout stderr)
          (loop (make-lexer (read-all-bytes stdin))))))





  (yaspl-test 'exit-code 'main #:exit-code 1)
  (yaspl-test 'exit-code2 'main #:exit-code 2)
  (yaspl-test 'exit-code3 'main #:exit-code 3)
  (yaspl-test 'exit-code4 'main #:exit-code 4)
  (yaspl-test 'exit-code5 'main #:exit-code 5)
  (yaspl-test 'exit-code6 'main #:exit-code 6)

  (yaspl-test 'stdout1 'main #:stdout #"Aa")

  (yaspl-test 'stdin1 'main #:stdin #"A" #:exit-code 65)
  (yaspl-test 'echo1 'main #:stdin #"Hello world" #:stdout #"Hello world")
  (yaspl-test 'echo2 'main #:stdin #"Hello world" #:stdout #"Hello world")
  (yaspl-test 'sum-tree 'main #:exit-code 15)


  (yaspl-test 'lexer 'main #:stdin #"((((" #:exit-code 0)
  (yaspl-test 'lexer 'main #:stdin #"()()()" #:exit-code 0)
  (yaspl-test 'lexer 'main #:stdin #"(()" #:exit-code 0)
  (yaspl-test 'lexer 'main #:stdin #"aaaa" #:exit-code 1)


  (yaspl-test 'sexp-parser 'main #:stdin #"" #:exit-code 1)
  (yaspl-test 'sexp-parser 'main #:stdin #"(" #:exit-code 1)
  (yaspl-test 'sexp-parser 'main #:stdin #")" #:exit-code 1)
  (yaspl-test 'sexp-parser 'main #:stdin #"()" #:exit-code 0)
  (yaspl-test 'sexp-parser 'main #:stdin #"(()" #:exit-code 1)
  (yaspl-test 'sexp-parser 'main #:stdin #"(()())" #:exit-code 0)
  (yaspl-test 'sexp-parser 'main #:stdin #"( ( ()(( )  )\n )( ))" #:exit-code 0)


  )
