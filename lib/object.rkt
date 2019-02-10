#lang racket/base

(require (only-in racket/class get-field new set-field!)
         "../object.rkt"
         "../private/error.rkt"
         "../private/function.rkt"
         "../private/object.rkt"
         "../private/this.rkt"
         (prefix-in ecma:
                    (combine-in
                     "../private/literal.rkt"
                     "../convert.rkt"
                     "../types.rkt")))

(provide get-properties)

(define (get-properties)
  `(["Object" . ,object-constructor]))

(define object-constructor
  (letrec
      ([call
        (λ args
          (apply construct args))]
       [construct
        (λ ([value 'undefined])
          (cond
            [(Object? value) value]
            [(or (string? value)
                 (boolean? value)
                 (number? value))
             (ecma:to-object value)]
            [else (new Object%)]))])
    (make-native-constructor call construct)))

(define (check-is-object o)
  (unless (Object? o)
    (raise-native-error 'type "not an object")))

(define-object-properties object-constructor
  ["prototype" Object:prototype]
  ["getPrototypeOf"
   (native-method (o)
     (check-is-object o)
     (get-field prototype o))]
  ["getOwnPropertyDescriptor"
   (native-method (o p)
     (from-property-descriptor
      (get-own-property o (ecma:to-string p))))]
  ["getOwnPropertyNames"
   (native-method (o)
     (check-is-object o)
     (apply
      ecma:array
      (hash-keys (get-field properties o))))]
  ["create"
   (native-method (o properties)
     (unless (or (ecma:null? o) (Object? o))
       (raise-native-error 'type "not an object or null"))
     (let ([obj (new Object% [prototype (if (ecma:null? o) #f o)])])
       (unless (eq? 'undefined properties)
         (for ([(pname pdesc) (in-hash
                               (get-field properties
                                (ecma:to-object properties)))]
               #:when (property-enumerable? pdesc))
           (define-own-property obj pname
                 (to-property-descriptor
                  (get-property-value obj pname))
                 #t)))
       obj))]
  ["defineProperty"
   (native-method (o p attributes)
     (check-is-object o)
     (define-own-property o (ecma:to-string p)
           (to-property-descriptor attributes)
           #t)
     o)]
  ["defineProperties"
   (native-method (o properties)
     (check-is-object o)
     (for ([(pname pdesc) (in-hash
                           (get-field properties
                            (ecma:to-object properties)))]
           #:when (property-enumerable? pdesc))
       (define-own-property o pname
             (to-property-descriptor
              (get-property-value o pname))
             #t))
     o)]
  ["seal"
   (native-method (o)
     (check-is-object o)
     (for ([prop (in-hash-values (get-field properties o))])
       (set-property-configurable?! prop #f))
     (set-field! extensible? o #f)
     o)]
  ["freeze"
   (native-method (o)
     (check-is-object o)
     (for ([prop (in-hash-values (get-field properties o))])
       (when (data-property? prop)
         (set-data-property-writable?! prop #f))
       (set-property-configurable?! prop #f))
     (set-field! extensible? o #f)
     o)]
  ["preventExtensions"
   (native-method (o)
     (check-is-object o)
     (set-field! extensible? o #f)
     o)]
  ["isSealed"
   (native-method (o)
     (check-is-object o)
     (and (not (get-field extensible? o))
          (for/and ([prop (in-hash-values (get-field properties o))])
            (not (property-configurable? prop)))))]
  ["isFrozen"
   (native-method (o)
     (check-is-object o)
     (and (not (get-field extensible? o))
          (for/and ([prop (in-hash-values (get-field properties o))])
            (and (not (property-configurable? prop))
                 (not (and (data-property? prop)
                           (data-property-writable? prop)))))))]
  ["isExtensible"
   (native-method (o)
     (check-is-object o)
     (get-field extensible? o))]
  ["keys"
   (native-method (o)
     (check-is-object o)
     (apply
      ecma:array
      (for/list ([(name prop) (in-hash
                               (get-field properties o))]
                 #:when (property-enumerable? prop))
        name)))])

(define-object-properties Object:prototype
  ["constructor" object-constructor]
  ["toString"
   (make-native-function
    (λ ()
      (cond
        [(eq? 'undefined this) "[object Undefined]"]
        [(eq? 'null this) "[object Null]"]
        [(format "[object ~a]"
                 (get-field class-name (ecma:to-object this)))])))]
  ["toLocaleString"
   (make-native-function
    (λ ()
      (define o (ecma:to-object this))
      (define f (get-property-value o "toString"))
      (unless (Function? f)
        (raise-native-error 'type "toString: not a function"))
      (apply/this f o '())))]
  ["valueOf"
   (make-native-function
    (λ ()
      (ecma:to-object this)))]
  ["hasOwnProperty"
   (make-native-function
    (λ (v)
      (property?
       (get-own-property
        (ecma:to-object this)
        (ecma:to-string v)))))]
  ["isPrototypeOf"
   (make-native-function
    (λ (v)
      (and (Object? v)
           (let ([o (ecma:to-object this)])
             (let loop ([v (get-field prototype v)])
               (and (not (eq? 'null v))
                    (or (eq? o v)
                        (loop (get-field prototype v)))))))))]
  ["propertyIsEnumerable"
   (make-native-function
    (λ (v)
      (define prop
        (get-own-property
         (ecma:to-object this)
         (ecma:to-string v)))
      (and (property? prop)
           (property-enumerable? prop))))])
