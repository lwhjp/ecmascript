#lang racket/base

(require racket/class
         "../private/function.rkt"
         "../private/object.rkt"
         "../private/types.rkt")

(provide (all-defined-out))

(define object-constructor
  (letrec
      ([call
        (λ (this . args)
          (apply construct args))]
       [construct
        (λ ([value 'undefined])
          (cond
            [(is-a? value ecma-object%) value]
            [(or (string? value)
                 (boolean? value)
                 (number? value))
             (to-object value)]
            [else
             (new ecma-object%
                  [prototype object-prototype]
                  [class "Object"])]))])
    (make-native-constructor call construct)))

(define-object-properties object-constructor
  ["prototype" object-prototype])

(define-object-properties object-prototype
  ["constructor" object-constructor])
