#lang racket

(require racket/syntax syntax/parse)

(define (syntax->string c)
  (let* ([s (open-output-string)]
         [l (syntax->list c)]
         [init-col (or (syntax-column (first l)) 0)]
         [col init-col]
         [line (or (syntax-line (first l)) 0)])
    (define (advance c init-line!)
      (let ([c (syntax-column c)]
            [l (syntax-line c)])
        (when (and l (l . > . line))
          (for-each (λ (_) (newline)) (range (- l line)))
          (set! line l)
          (init-line!))
        (when c
          (display (make-string (max 0 (- c col)) #\space))
          (set! col c))))
    (parameterize ([current-output-port s]
                   [read-case-sensitive #t])
      (define (loop init-line!)
        (lambda (c)
          (cond
            [(and (pair? (syntax-e c))
                  (eq? (syntax-e (car (syntax-e c))) 'quote))
             (advance c init-line!)
             (printf "'")
             (let ([i (cadr (syntax->list c))])
               (set! col (or (syntax-column i) col))
               ((loop init-line!) i))]
            [(pair? (syntax-e c))
             (advance c init-line!)
             (define c-paren-shape (syntax-property c 'paren-shape))
             (printf "~a" (or c-paren-shape #\())
             (set! col (+ col 1))
             (map (loop init-line!) (syntax->list c))
             (printf (case c-paren-shape
                       [(#\[) "]"]
                       [(#\{) "}"]
                       [else ")"]))
             (set! col (+ col 1))]
            [(vector? (syntax-e c))
             (advance c init-line!)
             (printf "#(")
             (set! col (+ col 2))
             (map (loop init-line!) (vector->list (syntax-e c)))
             (printf ")")
             (set! col (+ col 1))]
            [else
             (advance c init-line!)
             (let ([s (format "~s" (syntax-e c))])
               (set! col (+ col (string-length s)))
               (display s))])))
      (for-each (loop (lambda () (set! col init-col))) l))
    (get-output-string s)))


(define (fix-imports stx)
  (define (sort-syntax stx)
    (sort (syntax->list stx) symbol<?
          #:key
          (lambda (stx)
            (if (identifier? stx)
                (syntax-e stx)
                (syntax-e (first (syntax->list stx)))))))
  (datum->syntax #f
    (let ([column (syntax-column stx)]
          [line (syntax-line stx)])
      (for/list ([stx (sort-syntax stx)])
        (match (syntax->datum stx)
         [(list sym1 sym2)
          (define loc (list #f line column #f #f))
          (begin0
            (datum->syntax
              #f
              (begin
                (set! column (+ 1 column))
                (list
                  (begin0
                    (datum->syntax #f sym1 (list #f line column #f #f))
                    (set! column (+ 1 column (string-length (symbol->string sym1)))))
                  (begin0
                    (datum->syntax #f sym2 (list #f line column #f #f))
                    (set! column (+ column (string-length (symbol->string sym2)))))))
              loc)
            (set! column (+ 2 column)))]
         [sym
          (begin0
            (datum->syntax #f sym (list #f line column #f #f))
            (set! column (+ 1 column (string-length (symbol->string sym)))))])))
    stx))

(define (fix-imports2 stx)
  (define (sort-syntax stx)
    (sort (syntax->list stx) symbol<?
          #:key
          (lambda (stx)
            (if (identifier? stx)
                (syntax-e stx)
                (syntax-e (first (syntax->list stx)))))))
  (let ([column (syntax-column (first (syntax->list stx)))]
        [line (syntax-line (first (syntax->list stx)))])
    (for/list ([stx (sort-syntax stx)])
      (match (syntax->datum stx)
       [(list sym1 sym2)
        (define loc (list #f line column #f #f))
        (begin0
          (datum->syntax
            #f
            (begin
              (set! column (+ 1 column))
              (list
                (begin0
                  (datum->syntax #f sym1 (list #f line column #f #f))
                  (set! column (+ 1 column (string-length (symbol->string sym1)))))
                (begin0
                  (datum->syntax #f sym2 (list #f line column #f #f))
                  (set! column (+ column (string-length (symbol->string sym2)))))))
            loc)
          (set! column (+ 2 column)))]
       [sym
        (begin0
          (datum->syntax #f sym (list #f line column #f #f))
          (set! column (+ 1 column (string-length (symbol->string sym)))))]))))


(define (modified-form module-form)
  (syntax-case module-form ()
    [(module name
       imports-form
       exports
       types
       .
       definitions)
     (syntax-case #'imports-form ()
       [(imports imports-clauses ...)
        (let ()
          (define/with-syntax (modified-clauses ...)
            (for/list ([clause (in-syntax #'(imports-clauses ...))])
              (syntax-parse clause
                [(module-name
                   (~and #:types types-kw) (~and types-form (types ...))
                   (~and #:values values-kw) (~and values-form (values ...))
                   (~and #:patterns patterns-kw) (~and patterns-form (patterns ...)))
                 (quasisyntax/loc clause
                   (module-name
                     types-kw #,(fix-imports #'types-form)
                     values-kw #,(fix-imports #'values-form)
                     patterns-kw #,(fix-imports #'patterns-form)))]
                [(module-name . values)
                 (quasisyntax/loc clause
                   (module-name . #,(fix-imports2 #'values)))])))
          (quasisyntax/loc module-form
            (module name #,(syntax/loc #'imports-form (imports modified-clauses ...))
                    exports types . definitions)))])]))


(port-count-lines! (current-input-port))
(define module-form (read-syntax))
(void (write-string (syntax->string #`(#,(modified-form module-form)))))
(newline)
