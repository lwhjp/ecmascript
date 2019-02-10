#lang racket/base

(require (only-in racket/class send)
         "private/object.rkt")

(provide (all-defined-out)
         Object?)

(define (get-property-value object name)
  (send object get name))

(define (has-property? object name)
  (send object has-property? name))

(define (set-property-value! object name v [throw? #t])
  (send object put name v throw?))

(define (can-set-property? object name)
  (send object can-put? name))

(define (delete-property! object name [throw? #t])
  (send object delete name throw?))
