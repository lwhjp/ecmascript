#lang racket/base

(provide (all-defined-out)
         boolean?
         number?
         string?)

(define undefined 'undefined)

(define null 'null)

(define (defined? v)
  (not (eq? 'undefined v)))

(define (undefined? v)
  (eq? 'undefined v))

(define (null? v)
  (eq? 'null v))

(define (primitive-value? v)
  (or (undefined? v)
      (null? v)
      (boolean? v)
      (number? v)
      (string? v)))

(struct reference (base name strict?) #:transparent)
