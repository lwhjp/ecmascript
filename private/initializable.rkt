#lang typed/racket/base

(provide
 UninitializedValue
 uninitialized
 uninitialized?
 Initializable
 initialized?)

(struct uninitialized-value ()
  #:type-name UninitializedValue)

(define uninitialized (uninitialized-value))

(define uninitialized? uninitialized-value?)

(define-type (Initializable T) (U UninitializedValue T))

(define #:∀ (T) (initialized? [v : (Initializable T)])
  (not (uninitialized? v)))
