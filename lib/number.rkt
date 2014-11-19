#lang racket/base

(require math/flonum
         (except-in racket/class this)
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
  `(["Number" . ,number-constructor]))

(define number-constructor
  (letrec
      ([call
        (λ ([value 0])
          (ecma:to-number value))]
       [construct
        (λ ([value 0])
          (instantiate number% ((ecma:to-number value))
            [prototype number:prototype]))])
    (make-native-constructor call construct)))

(define-object-properties number-constructor
  ["prototype" number:prototype]
  ["MAX_VALUE" +max.0]
  ["MIN_VALUE" +min.0]
  ["NaN" +nan.0]
  ["NEGATIVE_INFINITY" -inf.0]
  ["POSITIVE_INFINITY" +inf.0])

(define-object-properties number:prototype
  ["constructor" number-constructor]
  ["toString"
   (native-method (radix)
     (if (= 10 (ecma:to-number radix))
         (ecma:to-string (get-field value this))
         (number->string
          (get-field value this)
          (ecma:to-number radix))))]
  ["toLocaleString"
   (native-method ()
     (number->string (get-field value this)))]
  ["valueOf"
   (native-method ()
     (get-field value this))]
  ["toFixed"
   (native-method (fractionDigits)
     (real->decimal-string
      (get-field value this)
      (ecma:to-integer fractionDigits)))]
  ["toExponential"
   (native-method (fractionDigits)
     (number->string (get-field value this)))]
  ["toPrecision"
   (native-method (precision)
     (number->string (get-field value this)))])
