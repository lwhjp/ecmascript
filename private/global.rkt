#lang racket/base

(require "boolean.rkt"
         "function.rkt"
         "math.rkt"
         "object.rkt"
         "string.rkt"
         "types.rkt")

(provide global-object)

(define object-constructor
  (letrec
      ([call (λ (this [value 'undefined])
               (if (or (eq? 'null value)
                       (eq? 'undefined value))
                   (new value)
                   (to-object value)))]
       [new (λ ([value 'undefined])
              (cond
                [(object? value) value]
                [(or (string? value)
                     (boolean? value)
                     (number? value))
                 (to-object value)]
                [else (object (object-class object-prototype-object)
                              object-prototype-object
                              (make-hash))]))])
    (make-native-constructor call new)))

(put! object-prototype-object "constructor" object-constructor)
(put! object-prototype-object "toString"
      (make-native-function
       (λ (this)
         (string-append "[object " (object-class this) "]"))))
(put! object-prototype-object "valueOf"
      (make-native-function
       (λ (this)
         this)))

(define function-constructor
  (letrec
      ([call (λ (this . args)
               (apply new args))]
       [new (λ args
              (error 'TODO))])
    (make-native-constructor call new)))

(put! function-prototype-object "constructor" function-constructor)

;; TODO: array

(define string-constructor
  (letrec
      ([call (case-lambda
               [(value) (to-string value)]
               [() ""])]
       [new (case-lambda
              [(value) (make-string-object (to-string value))]
              [() (make-string-object "")])])
    (make-native-constructor call new)))

(put! string-prototype-object "constructor" string-constructor)

(define boolean-constructor
  (letrec
      ([call (λ ([value #f])
               (to-boolean value))]
       [new (λ ([value #f])
              (make-boolean-object (to-boolean value)))])
    (make-native-constructor call new)))

(put! boolean-prototype-object "constructor" boolean-constructor)

(define global-object
  (object
   (object-class object-prototype-object)
   object-prototype-object
   (make-hash
    `(("NaN" . ,(make-property +nan.0))
      ("Infinity" . ,(make-property +inf.0))
      ; TODO: eval
      ; TODO: parseInt
      ; TODO: parseFloat
      ; TODO: escape
      ; TODO: unescape
      ; TODO: isNaN
      ; TODO: isFinite
      ("Object" . ,(make-property object-constructor))
      ("Function" . ,(make-property function-constructor))
      ; ("Array" . ,(make-property array-constructor))
      ("String" . ,(make-property string-constructor))
      ("Boolean" . ,(make-property boolean-constructor))
      ; ("Number" . ,(make-property number-constructor))
      ; ("Date" . ,(make-property date-constructor))
      ("Math" . ,(make-property math-object))))))
