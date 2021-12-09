#lang typed/racket/base

(require typed/racket/class
         typed/racket/unsafe)

(unsafe-require/typed racket/base
                      [(values bless) (All (A) (-> Any A))])

(provide (all-defined-out))

;; This really is unsafe: untyped code can set fields, which will not be caught by is-a?.
(define-syntax-rule
  (define-unsafe-class-predicate pred? class% Type<%>)
  (define-unsafe-predicate pred? (Instance Type<%>)
    (λ (v) (is-a? v (ann class% Type<%>)))))

(define-syntax-rule
  (define-unsafe-predicate pred? Type body)
  (define pred?
    ((inst bless (-> Any Boolean : Type))
     (ann body (-> Any Boolean)))))
