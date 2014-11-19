#lang racket/base

(require (except-in racket/class this)
         "../private/builtin.rkt"
         "../private/function.rkt"
         "../private/object.rkt"
         "../private/this.rkt"
         (prefix-in
          ecma:
          (combine-in
           "../convert.rkt"
           "../types.rkt")))

(provide get-properties)

(define (get-properties)
  `(["Boolean" . ,boolean-constructor]))

(define boolean-constructor
  (letrec
      ([call
        (λ ([value #f])
          (ecma:to-boolean value))]
       [construct
        (λ ([value #f])
          (instantiate boolean% ((ecma:to-boolean value))
            [prototype boolean:prototype]))])
    (make-native-constructor call construct)))

(define-object-properties boolean-constructor
  ["prototype" boolean:prototype])

(define-object-properties boolean:prototype
  ["constructor" boolean-constructor]
  ["toString"
   (native-method ()
     (if (get-field value this)
         "true"
         "false"))]
  ["valueOf"
   (native-method ()
     (get-field value this))])
