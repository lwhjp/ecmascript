#lang racket/base

(require racket/class
         "boolean.rkt"
         "function.rkt"
         "math.rkt"
         "object.rkt"
         "string.rkt"
         "types.rkt")

(provide current-global-scope
         global-object)

(define object-constructor
  (letrec
      ([call
        (λ (this [value 'undefined])
          (if (or (eq? 'null value)
                  (eq? 'undefined value))
              (construct value)
              (to-object value)))]
       [construct
        (λ ([value 'undefined])
          (cond
            [(is-a? value ecma-object%) value]
            [(or (string? value)
                 (boolean? value)
                 (number? value))
             (to-object value)]
            [else
             (new ecma-object%
                  [prototype object-prototype-object])]))])
    (make-native-constructor call construct)))

(void
 (send object-prototype-object
       put!
       "constructor"
       object-constructor)
 (send object-prototype-object
       put!
       "toString"
       (make-native-function
        (λ (this)
          (string-append
           "[object "
           (get-field class this)
           "]"))))
 (send object-prototype-object
       put!
       "valueOf"
       (make-native-function
        (λ (this)
          this))))

(define function-constructor
  (letrec
      ([call (λ (this . args)
               (apply new args))]
       [new (λ args
              (error 'TODO))])
    (make-native-constructor call new)))

(void
 (send function-prototype-object
       put!
       "constructor"
       function-constructor))

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

(void
 (send string-prototype-object
       put!
       "constructor"
       string-constructor))

(define boolean-constructor
  (letrec
      ([call (λ ([value #f])
               (to-boolean value))]
       [new (λ ([value #f])
              (make-boolean-object (to-boolean value)))])
    (make-native-constructor call new)))

(void
 (send boolean-prototype-object
       put!
       "constructor"
       boolean-constructor))

(define global-object
  (new ecma-object%
    [prototype #f]
    [class "Object"]
    [initial-properties
     `(("NaN" . ,(make-data-property +nan.0))
       ("Infinity" . ,(make-data-property +inf.0))
       ; TODO: eval
       ; TODO: parseInt
       ; TODO: parseFloat
       ; TODO: escape
       ; TODO: unescape
       ; TODO: isNaN
       ; TODO: isFinite
       ("Object" . ,(make-data-property object-constructor))
       ("Function" . ,(make-data-property function-constructor))
       ; ("Array" . ,(make-data-property array-constructor))
       ("String" . ,(make-data-property string-constructor))
       ("Boolean" . ,(make-data-property boolean-constructor))
       ; ("Number" . ,(make-data-property number-constructor))
       ; ("Date" . ,(make-data-property date-constructor))
       ("Math" . ,(make-data-property math-object)))]))

(define current-global-scope
  (make-parameter global-object))
