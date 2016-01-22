#lang racket/base

(provide (all-defined-out))

;; Information needed to compile other modules from this module
(struct module-signature (name exports types patterns) #:transparent)
(struct inductive-signature (module-name name type-args variants) #:transparent)
(struct variant-signature (name types) #:transparent)
