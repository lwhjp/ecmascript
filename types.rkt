#lang racket/base

(require "private/primitive.rkt")

(provide (all-defined-out)
         boolean?
         number?
         string?
 (rename-out
  [ecma:undefined undefined]
  [ecma:null null]
  [ecma:undefined? undefined?]
  [ecma:null? null?]))

(define (defined? v) (not (ecma:undefined? v)))

(define (primitive-value? v)
  (or (ecma:undefined? v)
      (ecma:null? v)
      (boolean? v)
      (number? v)
      (string? v)))
