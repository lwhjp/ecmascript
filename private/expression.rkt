#lang racket/base

(require (for-syntax racket/base)
         (only-in racket/class is-a? send)
         racket/provide
         racket/stxparam
         "environment.rkt"
         "error.rkt"
         "function.rkt"
         "object.rkt"
         "../function.rkt"
         "../types.rkt")

(provide (filtered-out
          (λ (name)
            (and (regexp-match? #rx"^ecma:" name)
                 (substring name 5)))
          (all-defined-out)))

(define (ecma:member obj id)
  (reference
   (to-object (get-value obj))
   (to-string (get-value id))
   #f))
