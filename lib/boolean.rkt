#lang racket/base

(require racket/class
         "../private/function.rkt"
         "../private/object.rkt"
         "../private/types.rkt")

(provide (all-defined-out))

(define boolean%
  (class ecma-object%
    (init-field value)
    (super-new [class "Boolean"])))

(define (make-boolean-object v)
  (unless (boolean? v)
    (raise-argument-error 'make-boolean-object "boolean?" v))
  (instantiate boolean% (v)
    [prototype boolean-prototype]))

(define boolean-prototype
  (instantiate boolean% (#f)
    [prototype object-prototype]))

(define boolean-constructor
  (letrec
      ([call (λ ([value #f])
               (to-boolean value))]
       [new (λ ([value #f])
              (make-boolean-object (to-boolean value)))])
    (make-native-constructor call new)))

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