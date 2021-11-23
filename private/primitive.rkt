#lang racket/base

(provide
 ecma:undefined
 ecma:undefined?
 ecma:null
 ecma:null?)

(struct opaque-primitive (name)
  #:methods gen:custom-write
  [(define (write-proc obj port mode)
     (define name (opaque-primitive-name obj))
     (case mode
       [(#f) (write name port)]
       [else (write-string (format "#<~a>" name) port)]))])

(define ecma:undefined (opaque-primitive 'undefined))

(define (ecma:undefined? v) (eq? ecma:undefined v))

(define ecma:null (opaque-primitive 'null))

(define (ecma:null? v) (eq? ecma:null v))
