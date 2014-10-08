#lang racket/base

(provide (all-defined-out))

(struct es (syntax) #:transparent)

(struct es-this es () #:transparent)
(struct es-id es (name) #:transparent)
(struct es-numeric es (value) #:transparent)
(struct es-string es (value) #:transparent)
(struct es-null es () #:transparent)
(struct es-bool es (value) #:transparent)

(struct es-member-expr es (object property) #:transparent)
(struct es-new-expr es (expr args) #:transparent)
(struct es-call-expr es (expr args) #:transparent)
(struct es-postfix-expr es (expr op) #:transparent)
(struct es-unary-expr es (op expr) #:transparent)
(struct es-binary-expr es (left op right) #:transparent)
(struct es-ternary-expr es (test true false) #:transparent)
(struct es-comma-expr es (left right) #:transparent)

(struct es-block es (statements) #:transparent)
(struct es-var-stmt es (decls) #:transparent)
(struct es-var-decl es (id expr) #:transparent)
(struct es-empty-stmt es () #:transparent)
(struct es-expr-stmt es (expr) #:transparent)
(struct es-if es (test true false) #:transparent)
(struct es-while es (test body) #:transparent)
(struct es-for es (init test step body) #:transparent)
(struct es-for-in es (init expr body) #:transparent)
(struct es-continue es () #:transparent)
(struct es-break es () #:transparent)
(struct es-return es (expr) #:transparent)
(struct es-with es (expr body) #:transparent)

(struct es-function es (name args body) #:transparent)

(struct es-program es (elements) #:transparent)
