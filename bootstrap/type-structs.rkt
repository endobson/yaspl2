#lang racket/base

(provide (all-defined-out))

(struct kind ())
(struct *-kind kind ())

(struct type () #:transparent)
(struct void-ty type () #:transparent)
(struct u8-ty type () #:transparent)
(struct s32-ty type () #:transparent)
(struct u32-ty type () #:transparent)
(struct s64-ty type () #:transparent)
(struct bytes-ty type () #:transparent)
(struct boolean-ty type () #:transparent)
(struct input-port-ty type () #:transparent)
(struct output-port-ty type () #:transparent)

;; Only used in typechecking as an expected type to indicate type inference
(struct unknown-ty type () #:transparent)

(struct box-ty-constructor type () #:transparent)
(struct box-ty type (v) #:transparent)
(struct array-ty-constructor type () #:transparent)
(struct array-ty type (v) #:transparent)

(struct fun-ty type (type-vars args result) #:transparent)

(struct type-var-ty type (v) #:transparent)

(struct data-ty type (module-name name args) #:transparent)
(struct data-ty-constructor type (module-name name arg-kinds) #:transparent)
