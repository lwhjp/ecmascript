#lang racket/base

(require "object.rkt")

(provide (all-defined-out))

(struct Boolean Object (value)
  #:property prop:class 'Boolean)

(define Boolean:prototype
  (Boolean Object:prototype (make-hash) #t #f))

(define (make-Boolean value)
  (Boolean Boolean:prototype (make-hash) #t value))

(struct Number Object (value)
  #:property prop:class 'Number)

(define Number:prototype
  (Number Object:prototype (make-hash) #t 0))

(define (make-Number value)
  (Number Number:prototype (make-hash) #t value))

(struct RegExp Object (pattern flags)
  #:property prop:class 'RegExp)

(struct String Object (value)
  #:property prop:class 'String)

(define String:prototype
  (String Object:prototype (make-hash) #t ""))

(define (make-String value)
  (String String:prototype (make-hash) #t value))
