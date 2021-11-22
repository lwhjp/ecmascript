#lang racket/base

(require (for-syntax racket/base
                     syntax/parse
                     syntax/stx)
         racket/class
         "../private/object.rkt"
         (only-in "../lib/array.rkt" make-array)
         (only-in "../lib/object.rkt" Object%)
         "../convert.rkt"
         (prefix-in ecma: "function.rkt")
         "reference.rkt")

(provide
 (all-defined-out)
 (rename-out [make-array array]))

(define-syntax (object stx)
  (define-syntax-class property-name
    #:attributes (name)
    (pattern v:str
      #:attr name (datum->syntax #'v (syntax-e #'v)))
    (pattern v:number
      #:attr name (datum->syntax #'v (number->string (syntax-e #'v)))))
  (define-splicing-syntax-class property-initializer
    #:attributes (desc)
    (pattern (~seq expr)
      #:attr desc #'`(data
                      (value . ,expr)
                      (writable . #t)
                      (enumerable . #t)
                      (configurable . #t)))
    (pattern (~seq #:get fn)
      #:attr desc #'`(accessor
                      (get . ,fn)
                      (enumerable . #t)
                      (configurable . #t)))
    (pattern (~seq #:set fn)
      #:attr desc #'`(accessor
                      (set . ,fn)
                      (enumerable . #t)
                      (configurable . #t))))
  (syntax-parse stx
    [(_ [name:property-name def:property-initializer] ...)
     #'(let ([obj (new Object%)])
         (define-own-property obj
           name.name
           def.desc
           #f)
         ...
         obj)]))

(define (regexp pattern flags)
  (ecma:new (identifier RegExp) pattern flags))
