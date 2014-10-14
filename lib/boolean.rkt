#lang racket/base

(require racket/class
         "../private/function.rkt"
         "../private/object.rkt"
         "../private/types.rkt")

(provide (all-defined-out))

(define boolean-constructor
  (letrec
      ([call
        (λ (this [value #f])
          (to-boolean value))]
       [construct
        (λ ([value #f])
          (make-boolean-object (to-boolean value)))])
    (make-native-constructor call construct)))

(define-object-properties boolean-constructor
  ["prototype" boolean-prototype])

(define-object-properties boolean-prototype
  ["constructor" boolean-constructor]
  ["toString"
   (native-method (this)
     (if (get-field value this)
         "true"
         "false"))]
  ["valueOf"
   (native-method (this)
     (get-field value this))])
