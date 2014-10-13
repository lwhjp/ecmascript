#lang racket/base

(require "../private/function.rkt"
         "../private/object.rkt")

(provide (all-defined-out))

(define function-constructor
  (letrec
      ([call
        (λ (this . args)
          (apply construct args))]
       [construct
        (λ args
          (error 'todo))])
    (make-native-constructor call construct)))

(define-object-properties function-constructor
  ["prototype" function-prototype])

(define-object-properties function-prototype
  ["constructor" function-constructor])
