#lang racket/base

(provide (all-defined-out))

(struct value ())
(struct function-val value (args env body))
(struct void-val value ())
(struct byte-val value (v))
(struct boolean-val value (v))
(struct bytes-val value (v))

(struct error-sentinal (info))

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
