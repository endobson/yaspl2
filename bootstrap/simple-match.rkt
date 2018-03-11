#lang racket/base

(require
  "machine-structs.rkt"
  "parser-structs.rkt"
  racket/list
  racket/syntax
  racket/match)

(provide compile-pattern)

(define (convert p pat-env temp-env val fail succ)
  (match p
    [(bytes-pattern& bytes)
     #`(if (equal? #,val #,bytes)
           #,succ
           #,fail)]
    [(byte-pattern& byte)
     #`(if (equal? #,val #,byte)
           #,succ
           #,fail)]
    [(variable-pattern& var)
     (define var-temp (hash-ref temp-env var))
     #`(let ([#,var-temp #,val])
         #,succ)]
    [(ignore-pattern&)
     succ]
    [(abstraction-pattern& pattern-binding pats)
     #`(if (equal? (variant-val-variant-name #,val) '#,(hash-ref pat-env pattern-binding))
           #,(for/fold ([succ succ]) ([pat (in-list pats)] [index (in-naturals)])
               (define field-temp (generate-temporary 'field))
               #`(let ([#,field-temp (vector-ref (variant-val-fields #,val) '#,index)])
                   #,(convert pat pat-env temp-env field-temp fail succ)))
           #,fail)]))

;; Pattern (Hash Symbol Symbol) (Listof Symbol) -> Syntax
;; The resulting syntax is a function that takes in a value and a success continuation and failure
;; continuation. The success continuation should be called with the values for the patterns with the
;; given symbols. The failure continuation takes no arguments.
(define (compile-pattern pat pat-env vars)
  (define temp-env
    (for/hash ([var (in-list vars)])
      (values var (generate-temporary var))))

  (define val (generate-temporary #'val))
  (define success (generate-temporary #'success))
  (define fail (generate-temporary #'fail))

  #`(lambda (#,val #,success #,fail)
      #,(convert pat pat-env temp-env val #`(#,fail)
                 #`(#,success #,@(for/list ([v (in-list vars)]) (hash-ref temp-env v))))))
