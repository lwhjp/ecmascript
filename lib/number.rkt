#lang racket/base

(require math/flonum
         racket/class
         racket/lazy-require
         "../private/object.rkt"
         "../private/this.rkt"
         "../private/primitive.rkt"
         (only-in "function.rkt" Function%)
         (only-in "object.rkt" Object:prototype)
         "util.rkt")

(lazy-require
 ["../convert.rkt" (to-integer to-number to-string)])

(provide get-properties
 make-Number)

(define (get-properties)
  `(["Number" . ,number-constructor]))

(define Number%
  (class ecma-object%
    (init [prototype Number:prototype])
    (init-field [value 0])
    (super-new [class-name 'Number]
               [prototype prototype])))

(define Number:prototype
  (new Number% [prototype Object:prototype]))

(define (make-Number value)
  (new Number% [value value]))

(define number-constructor
  (new
   (class Function%
     (super-new [formal-parameters '(value)]
                [proc to-number])
     (define/override (construct args)
       (let ([value (if (null? args) ecma:undefined (car args))])
         (make-Number (to-number value)))))))

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
     (if (= 10 (to-number radix))
         (to-string (get-field value ecma:this))
         (number->string
          (get-field value ecma:this)
          (to-number radix))))]
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
      (to-integer fractionDigits)))]
  ["toExponential"
   (native-method (fractionDigits)
     (number->string (get-field value ecma:this)))]
  ["toPrecision"
   (native-method (precision)
     (number->string (get-field value ecma:this)))])
