#lang racket/base

(provide (all-defined-out))

;; Information needed to compile other modules from this module
(struct module-signature (name exports types patterns statics) #:transparent)
(struct inductive-signature (module-name name type-args variants) #:transparent)
(struct prim-signature (type) #:transparent)
(struct variant-signature (name types) #:transparent)
(struct varargs-signature (type-vars arg-type return-type cons-mod cons-name empty-mod empty-name) #:transparent)
