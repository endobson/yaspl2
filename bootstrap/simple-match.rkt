#lang racket/base

(require
  "machine-structs.rkt"
  "parser-structs.rkt"
  racket/list
  racket/unsafe/ops
  racket/syntax
  racket/match)

(provide compile-pattern/simple-match)


(struct pat-fields (matcher-id bindings vars))

(define (convert p pat-env)
  (define (convert* p) (convert p pat-env))
    (match p
      [(bytes-pattern& bytes)
       (define matcher-id (generate-temporary))
       (define matcher
         #`(lambda (v sk fk) (if (and (bytes-val? v) (equal? (bytes-val-v v) #,bytes)) (sk) (fk))))
       (pat-fields
         matcher-id
         (list #`(#,matcher-id #,matcher))
         (list))]
      [(byte-pattern& byte)
       (define matcher-id (generate-temporary))
       (define matcher
         #`(lambda (v sk fk) (if (and (byte-val? v) (equal? (byte-val-v v) #,byte)) (sk) (fk))))
       (pat-fields
         matcher-id
         (list #`(#,matcher-id #,matcher))
         (list))]
      [(variable-pattern& var)
       (define matcher-id (generate-temporary))
       (define matcher #`(lambda (v sk fk) (sk v)))
       (pat-fields
         matcher-id
         (list #`(#,matcher-id #,matcher))
         (list (cons var (generate-temporary var))))]

      [(ignore-pattern&)
       (define matcher-id (generate-temporary))
       (define matcher #`(lambda (v sk fk) (sk)))
       (pat-fields
         matcher-id
         (list #`(#,matcher-id #,matcher))
         (list))]

      [(abstraction-pattern& pattern-binding pats)
       (define fields-list (map convert* pats))
       (define field-temps (generate-temporaries pats))
       (let-values
         ([(form vars)
           (let loop ([fields-list fields-list] [field-temps field-temps] [i 0] [vars empty])
             (if (empty? fields-list)
                 (values
                   #`(sk #,@(map cdr vars))
                   vars)
                 (match (first fields-list)
                   [(pat-fields matcher-id bindings new-vars)
                    (let-values ([(form full-vars)
                                  (loop (rest fields-list) (rest field-temps) (add1 i) (append new-vars vars))])
                      (values
                        #`(#,matcher-id
                           (unsafe-car
                             #,(for/fold ([expr #'(variant-val-fields v)]) ([_ (in-range i)])
                                 #`(unsafe-cdr #,expr)))
                           (lambda (#,@(map cdr new-vars)) #,form)
                           fk)
                        full-vars))])))])
         (define matcher
           #`(lambda (v sk fk)
               (if (equal? (variant-val-variant-name v) '#,(hash-ref pat-env pattern-binding))
                   #,form
                   (fk))))
         (define matcher-id (generate-temporary))
         (pat-fields
           matcher-id
           (cons #`(#,matcher-id #,matcher) (map pat-fields-bindings fields-list))
           vars))]))


;; Pattern (Hash Symbol Symbol) (Hash Symbol Indentifier) -> (Values Syntax (Hash Symbol Identifier))
(define (compile-pattern/simple-match pat pat-env env)
  (match (convert pat pat-env)
    [(pat-fields matcher-id bindings vars)
     (values
       #`(letrec (#,@(flatten bindings)) #,matcher-id)
       (map cdr vars)
       (for/fold ([env env]) ([var vars])
         (hash-set env (car var) (cdr var))))]))
