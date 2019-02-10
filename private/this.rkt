#lang racket/base

(require (for-syntax racket/base))

(provide (all-defined-out))

(define this-binding
  (make-parameter #f))

(define (apply/this this-arg proc . args)
  (parameterize ([this-binding this-arg])
    (apply apply proc args)))

(define-syntax this
  (λ (stx)
    (syntax-protect
     #'(this-binding))))
