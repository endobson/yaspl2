#lang racket/base

(provide (all-defined-out))

;; Information needed to compile other modules from this module
(struct module-signature (name exports types))
(struct inductive-signature (module-name name type-args variants))
(struct variant-signature (name types))
