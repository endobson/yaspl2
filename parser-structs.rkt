#lang racket/base

(provide (all-defined-out))

(struct module& (name imports exports types definitions))

(struct export& (name))
;; TODO change the order to match others
(struct imports& (types values patterns))
(struct import& (module-name name))
(struct definition& (type args body))

(struct expression& () #:transparent)
(struct byte& expression& (v) #:transparent)
(struct bytes& expression& (v) #:transparent)
(struct boolean& expression& (v) #:transparent)
(struct variable& expression& (v) #:transparent)
(struct if& expression& (cond true false) #:transparent)
(struct begin& expression& (first-expr exprs) #:transparent)
(struct app& expression& (op args) #:transparent)
(struct let& expression& (name expr body) #:transparent)
(struct case& expression& (expr clauses) #:transparent)
(struct case-clause& (pattern expr) #:transparent)

(struct pattern& ())
(struct bytes-pattern& pattern& (v))
(struct variable-pattern& pattern& (v))
(struct ignore-pattern& pattern& ())
(struct abstraction-pattern& pattern& (name patterns))

(struct define-type& (type-name type-variables variants))
(struct variant& (name fields))
(struct variant-field& (name type))

(struct pre-type () #:transparent)
(struct var-pre-type pre-type (v) #:transparent)
(struct fun-pre-type pre-type (type-vars args result) #:transparent)
(struct type-app-pre-type pre-type (constructor args))
