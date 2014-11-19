#lang racket/base

(require (only-in racket/class is-a? send)
         "types.rkt"
         "private/object.rkt")

(provide (all-defined-out))

(define (object? v)
  (is-a? v ecma-object%))

(define (get-property-value object name)
  (let ([property (get-property object name)])
    (cond
      [(data-property? property)
       (data-property-value property)]
      [(accessor-property? property)
       (let ([get (accessor-property-get property)])
         (if get
             (send get call object)
             undefined))]
      [else undefined])))

(define (has-property? object name)
  (and (get-property object name) #t))
