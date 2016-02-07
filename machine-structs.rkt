#lang racket/base

(provide (all-defined-out))

(struct value () #:transparent)
(struct function-val value (args env-box body) #:transparent)
(struct void-val value () #:transparent)
(struct byte-val value (v) #:transparent)
(struct boolean-val value (v) #:transparent)
(struct bytes-val value (v) #:transparent)
(struct box-val value (v) #:transparent)

(struct error-sentinal (info) #:transparent)

(struct prim-port-val value (port) #:transparent)
(struct prim-function-val value (name) #:transparent)

(struct variant-val (variant-name fields) #:transparent)
(struct variant-constructor-val (variant-name fields) #:transparent)
(struct field-accessor-val value (variant-name field-name) #:transparent)

(struct halt-k () #:transparent)
(struct apply-k (vals args env cont) #:transparent)
(struct if-k (true false env cont) #:transparent)
(struct ignore-k (expr env cont) #:transparent)
(struct bind-k (name expr env cont) #:transparent)
(struct case-k (clauses env cont) #:transparent)

(struct full-name (module-name main-name) #:transparent)
