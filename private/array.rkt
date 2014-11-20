#lang racket/base

(require "object.rkt")

(provide (all-defined-out))

(struct Array Object ()
  #:property prop:class "Array")

(define Array:prototype
  (Array Object:prototype (make-hash) #t))
