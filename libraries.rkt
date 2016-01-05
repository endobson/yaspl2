#lang racket/base

(require
  "interpreter.rkt"
  racket/set)
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
      (import (prim void make-bytes write-bytes read-bytes + * - bytes-length =)
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
        (let ([amount-written (write-bytes bytes out 0 (bytes-length bytes))])
          (if (= amount-written (bytes-length bytes))
              (void)
              (ponic "write-all-bytes: Didn't write all the bytes"))))

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
