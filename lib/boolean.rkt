#lang racket/base

(require racket/class
         "../private/function.rkt"
         "../private/object.rkt"
         (prefix-in ecma: "../types.rkt"))

(provide get-properties)

(define (get-properties)
  `(["Boolean" . ,boolean-constructor]))

(define boolean%
  (class ecma-object%
    (init-field value)
    (super-new [class "Boolean"])))

(define boolean-prototype
  (instantiate boolean% (#f)
    [prototype object-prototype]))

(define boolean-constructor
  (letrec
      ([call
        (λ (this [value #f])
          (ecma:to-boolean value))]
       [construct
        (λ ([value #f])
          (instantiate boolean% ((ecma:to-boolean value))
            [prototype boolean-prototype]))])
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
