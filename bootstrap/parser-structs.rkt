#lang racket/base

(provide (all-defined-out))

(struct module& (name imports exports types definitions))

(struct exports& (types values patterns))
(struct export& (in-name out-name))
;; TODO change the order to match others
(struct imports& (types values patterns))
(struct import& (module-name exported-name local-name))
(struct definition& (type args body))

(struct block& (definitions body) #:transparent)
(struct match-def& (pattern expr) #:transparent)

(struct expression& () #:transparent)
(struct byte& expression& (v) #:transparent)
(struct bytes& expression& (v) #:transparent)
(struct boolean& expression& (v) #:transparent)
(struct variable& expression& (v) #:transparent)
(struct if& expression& (cond true false) #:transparent)
(struct begin& expression& (first-expr exprs) #:transparent)
(struct app& expression& (op args) #:transparent)
(struct varargs-app& expression& (op args) #:transparent)
(struct let& expression& (name expr body) #:transparent)
(struct case& expression& (expr clauses) #:transparent)
(struct case-clause& (pattern body) #:transparent)
;; args+tys : (List (List Symbol PreType))
;; return : (U PreType #f)
(struct lambda& expression& (args+tys return body) #:transparent)

(struct pattern& () #:transparent)
(struct bytes-pattern& pattern& (v) #:transparent)
(struct byte-pattern& pattern& (v) #:transparent)
(struct variable-pattern& pattern& (v) #:transparent)
(struct ignore-pattern& pattern& () #:transparent)
(struct abstraction-pattern& pattern& (name patterns) #:transparent)

(struct define-type& (type-name type-variables variants))
(struct variant& (name fields))
(struct variant-field& (name type))

(struct pre-type () #:transparent)
(struct var-pre-type pre-type (v) #:transparent)
(struct fun-pre-type pre-type (type-vars args result) #:transparent)
(struct type-app-pre-type pre-type (constructor args))
