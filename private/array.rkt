#lang racket/base

(require racket/class
         "object.rkt")

(provide (all-defined-out))

(define Array%
  (class ecma-object%
    (init [prototype Array:prototype])
    (super-new [class-name 'Array]
               [prototype prototype])))

(define Array:prototype
  (new Array% [prototype Object:prototype]))

(define (Array? v) (is-a? v Array%))
