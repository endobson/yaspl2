#lang racket/base

(provide (all-defined-out))

(struct function-val (args env-box body) #:transparent)

(struct error-sentinal (info) #:transparent)

(struct variant-val (variant-name fields) #:transparent)
(struct variant-constructor-val (variant-name fields) #:transparent)
(struct field-accessor-val (variant-name field-name) #:transparent)

(struct full-name (module-name main-name) #:transparent)

;; Used in the compiled code to implement panic
(define exit-parameter (make-parameter #f))
