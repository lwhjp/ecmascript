#lang racket/base

(require math/flonum
         racket/class
         "../private/builtin.rkt"
         "../private/function.rkt"
         "../private/object.rkt"
         (prefix-in ecma: "../types.rkt"))

(provide get-properties)

(define (get-properties)
  `(["Number" . ,number-constructor]))

(define number-constructor
  (letrec
      ([call
        (λ (this [value 0])
          (ecma:to-number value))]
       [construct
        (λ ([value 0])
          (instantiate number% ((ecma:to-number value))
            [prototype number-prototype]))])
    (make-native-constructor call construct)))

(define-object-properties number-constructor
  ["prototype" number-prototype]
  ["MAX_VALUE" +max.0]
  ["MIN_VALUE" +min.0]
  ["NaN" +nan.0]
  ["NEGATIVE_INFINITY" -inf.0]
  ["POSITIVE_INFINITY" +inf.0])

(define-object-properties number-prototype
  ["constructor" number-constructor]
  ["toString"
   (native-method (this radix)
     (if (= 10 (ecma:to-number radix))
         (ecma:to-string (get-field value this))
         (number->string
          (get-field value this)
          (ecma:to-number radix))))]
  ["toLocaleString"
   (native-method (this)
     (number->string (get-field value this)))]
  ["valueOf"
   (native-method (this)
     (get-field value this))]
  ["toFixed"
   (native-method (this fractionDigits)
     (real->decimal-string
      (get-field value this)
      (ecma:to-integer fractionDigits)))]
  ["toExponential"
   (native-method (this fractionDigits)
     (number->string (get-field value this)))]
  ["toPrecision"
   (native-method (this precision)
     (number->string (get-field value this)))])
