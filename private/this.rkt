#lang racket/base

(require (for-syntax racket/base)
         "global-object.rkt"
         "../convert.rkt"
         "../types.rkt")

(provide (all-defined-out))

(define this-binding
  (make-parameter global-object))

(define (apply/this this-arg proc . args)
  (parameterize ([this-binding this-arg])
    (apply apply proc args)))

(define-syntax this
  (λ (stx)
    (syntax-protect
     #'(this-binding))))
