#lang racket/base

(require
  "machine-structs.rkt"
  "primitives.rkt"
  racket/list
  (only-in racket/contract/base and\/c)
  racket/set
  racket/match)
(provide
  run-program
  (struct-out program-result))

(struct module& (name imports exports types definitions))

(struct export& (name))
(struct import& (module-name name))
(struct definition& (args body))

(struct expression& ())
(struct byte& expression& (v))
(struct bytes& expression& (v))
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
    [(? (and/c bytes? immutable?) bytes) (bytes& bytes)]
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

    (for ([import (in-list (module&-imports module))])
      (hash-set! local-env (import&-name import)
                 (hash-ref global-env
                           (full-name (import&-module-name import) (import&-name import))
                           (λ ()
                              (error 'make-global-env "No export with name '~a' in ~a"
                                     (import&-name import) (import&-module-name import))))))

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
                 (hash-ref local-env name
                           (λ () (error 'make-global-env "No definition for export '~a' in ~a"
                                   (export&-name export) (module&-name module)))))))
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



(module+ modules
  (provide modules)
  (define modules (mutable-set))
  (define module-names (mutable-set))

  (define (add-module! mod-src)
    (define mod (parse-module mod-src))
    (define mod-name (module&-name mod))
    (when (set-member? module-names mod-name)
      (error 'add-module! "Cannot add module: ~s is already defined" mod-name))
    (set-add! module-names mod-name)
    (set-add! modules mod))

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
    '(module stdin2
       (import
         (numbers decimal-bytes->integer)
         (io read-all-bytes))
       (export main)
       (types)

       (define (main stdin stdout stderr)
         (decimal-bytes->integer (read-all-bytes stdin)))))


  (add-module!
    '(module panic1
       (import (prim panic make-bytes))
       (export main)
       (types)
       (define (main stdin stdout stderr)
         (panic (make-bytes 3)))))

  (add-module!
    '(module panic2
       (import (prim panic make-bytes))
       (export main)
       (types)
       (define (main stdin stdout stderr)
         (panic #"Boom"))))

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
        (import (prim + - = bytes-set! bytes-ref void make-bytes bytes-length))
        (export bytes-copy! bytes=? subbytes)
        (types)

        (define (bytes-copy! src s-start s-end dest d-start)
          (if (= s-start s-end)
              (void)
              (begin
                (bytes-set! dest d-start (bytes-ref src s-start))
                (bytes-copy! src (+ s-start 1) s-end dest (+ d-start 1)))))

        (define (subbytes src start end)
          (let ([new-bytes (make-bytes (- end start))])
            (begin
              (bytes-copy! src start end new-bytes 0)
              new-bytes)))

       (define (bytes=? b1 b2)
         (if (= (bytes-length b1) (bytes-length b2))
             (inner-bytes=? b1 b2 0)
             #f))
       (define (inner-bytes=? b1 b2 offset)
         (if (= offset (bytes-length b1))
             #t
             (if (= (bytes-ref b1 offset) (bytes-ref b2 offset))
                 (inner-bytes=? b1 b2 (+ 1 offset))
                 #f)))))


  (add-module!
    '(module numbers
        (import (prim and bytes-length bytes-ref bytes-set! + * - quotient remainder = <= < > panic
                      make-bytes void))
        (export digit? decimal-bytes->integer integer->decimal-bytes)
        (types)

        (define (digit? v)
          (and (<= 48 v) (< v 58)))

        (define (decimal-bytes->integer bytes)
          (decimal-bytes->integer/loop bytes 0 (bytes-length bytes) 0))

        (define (decimal-bytes->integer/loop bytes start end acc)
          (if (= start end)
              acc
              (let ([acc (+ (* 10 acc) (- (bytes-ref bytes start) 48))])
                (decimal-bytes->integer/loop bytes (+ 1 start) end acc))))

        (define (integer->decimal-bytes-length v)
          (if (< v 0)
              (+ 1 (integer->decimal-bytes-length (- 0 v)))
              (if (< v 10)
                  1
                  (+ 1 (integer->decimal-bytes-length (quotient v 10))))))

        (define (write-decimal-bytes v bytes offset)
          (if (< v 0)
              (begin
                (bytes-set! bytes 0 45) ;; '-'
                (write-decimal-bytes (- 0 v) bytes offset))
              (let ([b (remainder v 10)])
                (begin
                  (bytes-set! bytes offset (+ b 48))
                  (let ([v (quotient v 10)])
                    (if (> v 0)
                        (write-decimal-bytes v bytes (- offset 1))
                        (void)))))))

        (define (integer->decimal-bytes v)
          (let ([len (integer->decimal-bytes-length v)])
            (let ([bytes (make-bytes len)])
              (begin
                (write-decimal-bytes v bytes (- len 1))
                bytes))))))



  (add-module!
    '(module io
        (import (prim make-bytes read-bytes + * - bytes-length =)
                (bytes-output write-bytes)
                (bytes bytes-copy!))
        (export read-all-bytes write-all-bytes write-newline)
        (types)

        (define (read-all-bytes in)
          (read-all-bytes-loop in (make-bytes 1) 0))

        (define (read-all-bytes-loop in buf cur-size)
          (let ([amount-read (read-bytes buf in cur-size (bytes-length buf))])
            (if (= amount-read 0)
                (let ([trim-bytes (make-bytes cur-size)])
                   (begin
                     (bytes-copy! buf 0 cur-size trim-bytes 0)
                     trim-bytes))
                (let ([new-size (+ amount-read cur-size)])
                  (if (= new-size (bytes-length buf))
                      (let ([new-buf (make-bytes (* 2 (bytes-length buf)))])
                        (begin
                          (bytes-copy! buf 0 new-size new-buf 0)
                          (read-all-bytes-loop in new-buf new-size)))
                      (read-all-bytes-loop in buf new-size))))))

        (define (write-all-bytes bytes out)
          (write-bytes bytes out 0 (bytes-length bytes)))
        (define (write-newline out)
          (write-all-bytes #"\n" out))


        ))



  (add-module!
    '(module list
        (import)
        (export cons empty cons-head cons-tail reverse)
        (types
          (define-type (List a)
            (cons [head a] [tail (List a)])
            (empty)))

        (define (reverse list)
          (reverse-helper list (empty)))
        (define (reverse-helper l1 l2)
          (case l1
            [(empty) l2]
            [(cons hd tl) (reverse-helper tl (cons hd l2))]))))

  (add-module!
    '(module either
        (import)
        (export left right right-v)
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
               (numbers decimal-bytes->integer)
               (io read-all-bytes)
               (prim void panic)
               (either left right)
               (list cons empty reverse))
       (export parse-sexp main)
       (types
         (define-type Sexp
            (node [list List])
            (symbol-sexp [bytes Bytes])
            (number-sexp [byte Byte]))

         (define-type SexpResult
            (sexp-result [v Sexp] [lexer Lexer])
            (sexp-result-error)))



       (define (parse-sexp bytes)
         (let ([lexer (make-lexer bytes)])
           (let ([val (loop lexer)])
             (case val
               [(sexp-result v lexer)
                 (case (run-lexer lexer)
                   [(lex-result v lexer) (left #"Leftovers")]
                   [(bad-input) (left #"Bad input")]
                   [(end-of-input) (right v)])]
               [(sexp-result-error) (left #"Sexp result error")]))))

       (define (loop lexer)
         (let ([val (run-lexer lexer)])
           (case val
             [(end-of-input) (sexp-result-error)]
             [(bad-input) (sexp-result-error)]
             [(lex-result v lexer)
               (case v
                 [(number-lexeme bytes) (sexp-result (number-sexp (decimal-bytes->integer bytes)) lexer)]
                 [(symbol-lexeme bytes) (sexp-result (symbol-sexp bytes) lexer)]
                 [(left-paren-lexeme) (node-loop (empty) lexer)]
                 [(right-paren-lexeme) (sexp-result-error)])])))

       (define (node-loop vals lexer)
         (let ([val (run-lexer lexer)])
           (case val
             [(end-of-input) (sexp-result-error)]
             [(bad-input) (sexp-result-error)]
             [(lex-result v lexer)
               (case (lex-result-v val)
                 [(symbol-lexeme bytes)
                  (node-loop (cons (symbol-sexp bytes) vals) lexer)]
                 [(number-lexeme bytes)
                  (node-loop (cons (number-sexp (decimal-bytes->integer bytes)) vals) lexer)]
                 [(left-paren-lexeme)
                   (case (node-loop (empty) lexer)
                     [(sexp-result v lexer)
                      (node-loop (cons v vals) lexer)]
                     [(sexp-result-error) (sexp-result-error)])]
                 [(right-paren-lexeme) (sexp-result (node (reverse vals)) lexer)])])))

       (define (main stdin stderr stdout)
         (let ([result (parse-sexp (read-all-bytes stdin))])
           (case result
             [(right v) 0]
             [(left v) (panic v)])))))




  (add-module!
    '(module lexer
        (import (prim + = make-bytes read-bytes bytes-length bytes-ref or)
                (numbers digit?)
                (bytes subbytes)
                (io read-all-bytes))
        (export main make-lexer run-lexer lex-result-v lex-result-next )
        (types
          (define-type Lexer
            (lexer [input Bytes] [pos Byte]))

          (define-type Lexeme
            (left-paren-lexeme)
            (right-paren-lexeme)
            (symbol-lexeme [v Bytes])
            (number-lexeme [v Bytes]))

          (define-type Result
            (lex-result [v lexeme] [next Lexer])
            (end-of-input)
            (bad-input)))

        #;
        (define (digit->number v)
          (- v 48))

        ;; Space or Newline
        (define (whitespace? v)
          (or (= v 32) (= v 10)))


        (define (number-start-byte? v)
          (digit? v))
        (define (number-continue-byte? v)
          (digit? v))

        (define (symbol-start-byte? v)
          (or (= v 42)
              (or (= v 43)
                  (or (= v 45)
                      (= v 47)))))
        (define (symbol-continue-byte? v)
          #f)

        (define (lex-symbol bytes start cur)
          (if (= (bytes-length bytes) cur)
              (lex-result (symbol-lexeme (subbytes bytes start cur))
                          (lexer bytes cur))
              (if (symbol-continue-byte? (bytes-ref bytes cur))
                  (lex-symbol bytes start (+ 1 cur))
                  (lex-result (symbol-lexeme (subbytes bytes start cur))
                              (lexer bytes cur)))))

        (define (lex-number bytes start cur)
          (if (= (bytes-length bytes) cur)
              (lex-result (number-lexeme (subbytes bytes start cur))
                          (lexer bytes cur))
              (if (number-continue-byte? (bytes-ref bytes cur))
                  (lex-number bytes start (+ 1 cur))
                  (lex-result (number-lexeme (subbytes bytes start cur))
                              (lexer bytes cur)))))



        (define (make-lexer bytes)
          (lexer bytes 0))

        (define (run-lexer lexer)
          (run (lexer-input lexer) (lexer-pos lexer)))


        (define (run bytes pos)
          (if (= pos (bytes-length bytes))
              (end-of-input)
              (let ([byte (bytes-ref bytes pos)])
                (if (= byte 40)
                    (lex-result (left-paren-lexeme) (lexer bytes (+ pos 1)))
                    (if (= byte 41)
                        (lex-result (right-paren-lexeme) (lexer bytes (+ pos 1)))
                        (if (whitespace? byte)
                            (run bytes (+ 1 pos))
                            (if (symbol-start-byte? byte)
                                (lex-symbol bytes pos (+ pos 1))
                                (if (number-start-byte? byte)
                                    (lex-number bytes pos (+ pos 1))
                                    (bad-input)))))))))

        (define (loop lexer)
          (case (run-lexer lexer)
            [(lex-result v lexer) (loop lexer)]
            [(end-of-input) 0]
            [(bad-input) 1]))

        (define (main stdin stdout stderr)
          (loop (make-lexer (read-all-bytes stdin))))))


  (add-module!
    '(module arithmetic-expr
       (import
         (prim panic)
         (sexp-parser parse-sexp)
         (io read-all-bytes)
         (either right-v)
         (bytes bytes=?))
       (export parse-arith-expr main)
       (types
         (define-type ArithExpr
           (num-lit [v Byte])
           (num-op-expr [v NumOp] [left arith-expr] [right arith-expr]))
         (define-type NumOp
           (plus-op)
           (minus-op)
           (times-op)))

       (define (parse-arith-expr sexp)
         (case sexp
           [(node ops)
            (case ops
              [(cons sym ops)
               (case ops
                 [(cons arg1 ops)
                  (case ops
                    [(cons arg2 ops)
                     (case ops
                       [(empty) (parse-arith/fun-two-args sym arg1 arg2)]
                       [(cons arg3 ops) (panic #"Too many arguments")])]
                    [(empty) (panic #"Too few arguments: got 1")])]
                 [(empty) (panic #"Too few arguments: got 0")])]
              [(empty) (panic #"No function symbol")])]
           [(symbol-sexp bytes) (panic #"Symbols not supported")]
           [(number-sexp byte) (num-lit byte)]))

       (define (parse-arith/fun-two-args sym arg1 arg2)
         (let ([arg1-parsed (parse-arith-expr arg1)])
           (let ([arg2-parsed (parse-arith-expr arg2)])
             (case sym
               [(node args) (panic #"Function position is a node")]
               [(number-sexp v) (panic #"Function position is a number")]
               [(symbol-sexp bytes)
                (num-op-expr (bytes->num-op bytes) arg1-parsed arg2-parsed)]))))

       (define (bytes->num-op bytes)
         (if (bytes=? bytes #"+")
             (plus-op)
             (if (bytes=? bytes #"-")
                 (minus-op)
                 (if (bytes=? bytes #"*")
                     (times-op)
                     (panic #"Unknown op")))))

       (define (main stdin stdout stderr)
         (begin
           (parse-arith-expr (right-v (parse-sexp (read-all-bytes stdin))))
           0))))



  (add-module!
    '(module stack-machine
       (import
         (prim void)
         (arithmetic-expr parse-arith-expr)
         (list cons empty cons-head)
         (sexp-parser parse-sexp)
         (io read-all-bytes write-all-bytes write-newline)
         (bytes-output write-bytes)
         (numbers integer->decimal-bytes)
         (either right-v))
       (export main compile-arith-expr stack-function-blocks stack-basic-block-cmds)
       (types
         (define-type StackCmd
           (num-lit-cmd [v Byte])
           (eval-op-cmd [v NumOp]))
         (define-type StackTerminal
           (return))
         (define-type StackBasicBlock
           (stack-basic-block [cmds (List StackCmd)] [terminal StackTerminal]))
         (define-type StackFunction
           (stack-function [name Bytes] [blocks (List StackBasicBlock)])))

       (define (compile-arith-expr expr)
         (stack-function #"main"
                         (cons (stack-basic-block (compile-arith-expr/loop expr (empty)) (return))
                               (empty))))

       (define (compile-arith-expr/loop expr cmds)
         (case expr
           [(num-op-expr op left right)
            (compile-arith-expr/loop left (compile-arith-expr/loop right (cons (eval-op-cmd op) cmds)))]
           [(num-lit v)
            (cons (num-lit-cmd v) cmds)]))

       (define (print-function sfun output)
         (print-cmds (stack-basic-block-cmds (cons-head (stack-function-blocks sfun))) output))

       (define (print-cmds cmds output)
         (case cmds
           [(empty) (void)]
           [(cons cmd cmds)
            (case cmd
              [(num-lit-cmd v)
               (begin
                 (write-all-bytes (integer->decimal-bytes v) output)
                 (write-newline output)
                 (print-cmds cmds output))]
              [(eval-op-cmd op)
               (begin
                 (write-all-bytes (num-op->bytes op) output)
                 (write-newline output)
                 (print-cmds cmds output))])]))

       (define (num-op->bytes op)
         (case op
           [(plus-op) #"+"]
           [(minus-op) #"-"]
           [(times-op) #"*"]))

       (define (main stdin stdout stderr)
         (begin
           (print-function
             (compile-arith-expr (parse-arith-expr (right-v (parse-sexp (read-all-bytes stdin)))))
             stdout)
           0))))


  (add-module!
    '(module x86-64-stack-machine
       (import
         (prim bytes-length make-bytes * + -)
         (list cons-head)
         (numbers integer->decimal-bytes)
         (io read-all-bytes write-all-bytes write-newline)
         (arithmetic-expr parse-arith-expr)
         (sexp-parser parse-sexp)
         (bytes bytes-copy! subbytes)
         (bytes-output write-bytes)
         (stack-machine compile-arith-expr stack-function-blocks stack-basic-block-cmds)
         (either right-v))
       (export main)
       (types)

       (define (compile-stack-machine sfun)
         (let ([cmds (stack-basic-block-cmds (cons-head (stack-function-blocks sfun)))])
           (let ([bytes (make-bytes (* 64 64))])
             (let ([offset 0])
               (let ([offset (write-start bytes offset)])
                 (let ([offset (write-main-header bytes offset)])
                   (let ([offset (compile-stack-machine/loop cmds bytes offset)])
                     (let ([offset (write-main-footer bytes offset)])
                       (subbytes bytes 0 offset)))))))))

       (define (write-start bytes offset)
         (let ([prologue #".section __TEXT,__text\n\n.global _start\n_start:\ncall main\nmovq $0x2000001, %rax\npop %rdi\nsyscall\n"])
           (begin
             (bytes-copy! prologue 0 (bytes-length prologue) bytes offset)
             (+ (bytes-length prologue) offset))))

       (define (write-main-header bytes offset)
         (let ([header #"main:\n"])
           (begin
             (bytes-copy! header 0 (bytes-length header) bytes offset)
             (+ (bytes-length header) offset))))
       (define (write-main-footer bytes offset)
         (let ([footer #"pop %rax\npop %rbx\npush %rax\njmpq *%rbx"])
           (begin
             (bytes-copy! footer 0 (bytes-length footer) bytes offset)
             (+ (bytes-length footer) offset))))





       (define (compile-stack-machine/loop cmds bytes offset)
         (case cmds
           [(empty) offset]
           [(cons cmd cmds)
            (case cmd
              [(num-lit-cmd v)
               (let ([initial-offset offset])
                 (begin
                   (bytes-copy! #"push $" 0 6 bytes offset)
                   (let ([offset (+ 6 offset)])
                     (let ([decimal-number (integer->decimal-bytes v)])
                       (begin
                         (bytes-copy! decimal-number 0 (bytes-length decimal-number) bytes offset)
                         (let ([offset (+ offset (bytes-length decimal-number))])
                           (begin
                             (bytes-copy! #"\n" 0 1 bytes offset)
                             (let ([offset (+ 1 offset)])
                               (compile-stack-machine/loop cmds bytes offset)))))))))]
              [(eval-op-cmd op)
               (let ([op-bytes (case op
                                 [(plus-op) #"pop %rbx\npop %rax\naddq %rbx, %rax\npush %rax\n"]
                                 [(times-op) #"pop %rbx\npop %rax\nmulq %rbx\npush %rax\n"]
                                 [(minus-op) #"pop %rbx\npop %rax\nsubq %rbx, %rax\npush %rax\n"])])
                 (begin
                   (bytes-copy! op-bytes 0 (bytes-length op-bytes) bytes offset)
                   (compile-stack-machine/loop cmds bytes (+ offset (bytes-length op-bytes)))))])]))


       (define (main stdin stdout stderr)
         (begin
           (write-all-bytes
             (compile-stack-machine
               (compile-arith-expr
                 (parse-arith-expr
                   (right-v (parse-sexp (read-all-bytes stdin))))))
             stdout)
           0))))
  )








(module+ stack-machine-driver
  (require
    (submod ".." modules)
    racket/port)
  (define stdin (port->bytes (current-input-port)))
  (let ([result (run-program modules 'x86-64-stack-machine 'main #:stdin stdin)])
    (write-bytes (program-result-stdout result) (current-output-port))
    (write-bytes (program-result-stderr result) (current-error-port))
    (exit (program-result-exit-code result)))
         )

