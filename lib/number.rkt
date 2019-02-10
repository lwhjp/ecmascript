#lang racket/base

(require math/flonum
         (only-in racket/class get-field)
         "../private/builtin.rkt"
         "../private/function.rkt"
         "../private/object.rkt"
         "../private/this.rkt"
         (prefix-in
          ecma:
          (combine-in
           "../convert.rkt")))

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
          (make-Number (ecma:to-number value)))])
    (make-native-constructor call construct)))

(define-object-properties number-constructor
  ["prototype" Number:prototype]
  ["MAX_VALUE" +max.0]
  ["MIN_VALUE" +min.0]
  ["NaN" +nan.0]
  ["NEGATIVE_INFINITY" -inf.0]
  ["POSITIVE_INFINITY" +inf.0])

(define-object-properties Number:prototype
  ["constructor" number-constructor]
  ["toString"
   (native-method (radix)
     (if (= 10 (ecma:to-number radix))
         (ecma:to-string (get-field value ecma:this))
         (number->string
          (get-field value ecma:this)
          (ecma:to-number radix))))]
  ["toLocaleString"
   (native-method ()
     (number->string (get-field value ecma:this)))]
  ["valueOf"
   (native-method ()
     (get-field value ecma:this))]
  ["toFixed"
   (native-method (fractionDigits)
     (real->decimal-string
      (get-field value ecma:this)
      (ecma:to-integer fractionDigits)))]
  ["toExponential"
   (native-method (fractionDigits)
     (number->string (get-field value ecma:this)))]
  ["toPrecision"
   (native-method (precision)
     (number->string (get-field value ecma:this)))])
