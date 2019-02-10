#lang racket/base

(require (for-syntax racket/base))

(provide (all-defined-out))

(define current-ecma:this (make-parameter #f))

(define-syntax (ecma:this stx)
  (syntax-case stx ()
    [_ (identifier? stx) (syntax/loc stx (current-ecma:this))]))

(define (apply/this this-arg proc args)
  (parameterize ([current-ecma:this this-arg])
    (apply proc args)))
