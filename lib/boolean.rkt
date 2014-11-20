#lang racket/base

(require "../private/builtin.rkt"
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
          (make-Boolean (ecma:to-boolean value)))])
    (make-native-constructor call construct)))

(define-object-properties boolean-constructor
  ["prototype" Boolean:prototype])

(define-object-properties Boolean:prototype
  ["constructor" boolean-constructor]
  ["toString"
   (native-method ()
     (if (Boolean-value this)
         "true"
         "false"))]
  ["valueOf"
   (native-method ()
     (Boolean-value this))])
