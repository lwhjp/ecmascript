#lang racket/base

(provide
 ecma:undefined
 ecma:undefined?
 ecma:null
 ecma:null?)

(struct opaque-primitive (name))

(define ecma:undefined (opaque-primitive 'undefined))

(define (ecma:undefined? v) (eq? ecma:undefined v))

(define ecma:null (opaque-primitive 'null))

(define (ecma:null? v) (eq? ecma:null v))
