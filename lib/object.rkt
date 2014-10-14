#lang racket/base

(require racket/class
         "../private/function.rkt"
         "../private/object.rkt"
         "../private/types.rkt")

(provide (all-defined-out))

(define object-constructor
  (letrec
      ([call
        (λ (this . args)
          (apply construct args))]
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
                  [prototype object-prototype]
                  [class "Object"])]))])
    (make-native-constructor call construct)))

(define-object-properties object-constructor
  ["prototype" object-prototype])

(define-object-properties object-prototype
  ["constructor" object-constructor]
  ["toString"
   (make-native-function
    (λ (this)
      (cond
        [(eq? 'undefined this) "[object Undefined]"]
        [(eq? 'null this) "[object Null]"]
        [(format "[object ~a]"
                 (get-field class (to-object this)))])))]
  ["toLocaleString"
   (make-native-function
    (λ (this)
      (define o (to-object this))
      (define f (send o get "toString"))
      (unless (is-a? f function%)
        (error "type error"))
      (send f call o)))]
  ["valueOf"
   (make-native-function
    (λ (this)
      (to-object this)))]
  ["hasOwnProperty"
   (make-native-function
    (λ (this v)
      (property?
       (send (to-object this)
             get-own-property
             (to-string v)))))]
  ["isPrototypeOf"
   (make-native-function
    (λ (this v)
      (and (is-a? v ecma-object%)
           (let ([o (to-object this)])
             (let loop ([v (get-field prototype v)])
               (and (not (eq? 'null v))
                    (or (eq? o v)
                        (loop (get-field prototype v)))))))))]
  ["propertyIsEnumerable"
   (make-native-function
    (λ (this v)
      (define prop
        (send (to-object this)
              get-own-property
              (to-string v)))
      (and (property? prop)
           (property-enumerable? prop))))])
