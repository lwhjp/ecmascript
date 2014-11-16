#lang racket/base

(require (only-in racket/class is-a?)
         "private/object.rkt")

(provide object?)

(define (object? v)
  (is-a? v ecma-object%))
