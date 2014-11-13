#lang racket/base

(require racket/class
         "../private/error.rkt"
         "../private/function.rkt"
         "../private/object.rkt"
         (prefix-in ecma:
                    (combine-in
                     "../private/literal.rkt"
                     "../types.rkt")))

(provide get-properties)

(define (get-properties)
  `(["Object" . ,object-constructor]))

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
             (ecma:to-object value)]
            [else
             (new ecma-object%
                  [prototype object-prototype]
                  [class "Object"])]))])
    (make-native-constructor call construct)))

(define (check-is-object o)
  (unless (is-a? o ecma-object%)
    (raise-native-error 'type "not an object")))

(define-object-properties object-constructor
  ["prototype" object-prototype]
  ["getPrototypeOf"
   (native-method (this o)
     (check-is-object o)
     (get-field prototype o))]
  ["getOwnPropertyDescriptor"
   (native-method (this o p)
     (from-property-descriptor
      (send o get-own-property (ecma:to-string p))))]
  ["getOwnPropertyNames"
   (native-method (this o)
     (check-is-object o)
     (apply
      ecma:array
      (hash-keys (get-field properties o))))]
  ["create"
   (native-method (this o properties)
     (unless (or (null? o) (object? o))
       (raise-native-error 'type "not an object or null"))
     (let ([obj (new ecma-object%
                     [class "Object"]
                     [prototype (if (null? o)
                                    #f
                                    o)])])
       (unless (eq? 'undefined properties)
         (for ([(pname pdesc) (in-hash
                               (get-field properties
                                          (ecma:to-object properties)))]
               #:when (property-enumerable? pdesc))
           (send obj define-own-property pname
                 (to-property-descriptor
                  (send obj get pname))
                 #t)))
       obj))]
  ["defineProperty"
   (native-method (this o p attributes)
     (check-is-object o)
     (send o define-own-property (ecma:to-string p)
           (to-property-descriptor attributes)
           #t)
     o)]
  ["defineProperties"
   (native-method (this o properties)
     (check-is-object o)
     (for ([(pname pdesc) (in-hash
                           (get-field properties
                                      (ecma:to-object properties)))]
           #:when (property-enumerable? pdesc))
       (send o define-own-property pname
             (to-property-descriptor
              (send o get pname))
             #t))
     o)]
  ["seal"
   (native-method (this o)
     (check-is-object o)
     (for ([prop (in-hash-values (get-field properties o))])
       (set-property-configurable?! prop #f))
     (set-field! extensible? o #f)
     o)]
  ["freeze"
   (native-method (this o)
     (check-is-object o)
     (for ([prop (in-hash-values (get-field properties o))])
       (when (data-property? prop)
         (set-data-property-writable?! prop #f))
       (set-property-configurable?! prop #f))
     (set-field! extensible? o #f)
     o)]
  ["preventExtensions"
   (native-method (this o)
     (check-is-object o)
     (set-field! extensible? o #f)
     o)]
  ["isSealed"
   (native-method (this o)
     (check-is-object o)
     (and (not (get-field extensible? o))
          (for/and ([prop (in-hash-values (get-field properties o))])
            (not (property-configurable? prop)))))]
  ["isFrozen"
   (native-method (this o)
     (check-is-object o)
     (and (not (get-field extensible? o))
          (for/and ([prop (in-hash-values (get-field properties o))])
            (and (not (property-configurable? prop))
                 (not (and (data-property? prop)
                           (data-property-writable? prop)))))))]
  ["isExtensible"
   (native-method (this o)
     (check-is-object o)
     (get-field extensible? o))]
  ["keys"
   (native-method (this o)
     (check-is-object o)
     (apply
      ecma:array
      (for/list ([(name prop) (in-hash
                               (get-field properties o))]
                 #:when (property-enumerable? prop))
        name)))])

(define-object-properties object-prototype
  ["constructor" object-constructor]
  ["toString"
   (make-native-function
    (λ (this)
      (cond
        [(eq? 'undefined this) "[object Undefined]"]
        [(eq? 'null this) "[object Null]"]
        [(format "[object ~a]"
                 (get-field class (ecma:to-object this)))])))]
  ["toLocaleString"
   (make-native-function
    (λ (this)
      (define o (ecma:to-object this))
      (define f (send o get "toString"))
      (unless (is-a? f function%)
        (raise-native-error 'type "toString: not a function"))
      (send f call o)))]
  ["valueOf"
   (make-native-function
    (λ (this)
      (ecma:to-object this)))]
  ["hasOwnProperty"
   (make-native-function
    (λ (this v)
      (property?
       (send (ecma:to-object this)
             get-own-property
             (ecma:to-string v)))))]
  ["isPrototypeOf"
   (make-native-function
    (λ (this v)
      (and (is-a? v ecma-object%)
           (let ([o (ecma:to-object this)])
             (let loop ([v (get-field prototype v)])
               (and (not (eq? 'null v))
                    (or (eq? o v)
                        (loop (get-field prototype v)))))))))]
  ["propertyIsEnumerable"
   (make-native-function
    (λ (this v)
      (define prop
        (send (ecma:to-object this)
              get-own-property
              (ecma:to-string v)))
      (and (property? prop)
           (property-enumerable? prop))))])
