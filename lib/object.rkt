#lang racket/base

(require racket/class
         racket/lazy-require
         "../private/error.rkt"
         "../private/function.rkt"
         "../private/object.rkt"
         "../private/primitive.rkt"
         "../private/this.rkt"
         "util.rkt")

(lazy-require
 ["array.rkt" (make-array)]
 ["../convert.rkt" (to-object to-string)])

(provide get-properties
 Object%
 Object:prototype)

(define (get-properties)
  `(["Object" . ,object-constructor]))

(define Object%
  (class ecma-object%
    (init [prototype Object:prototype])
    (super-new [class-name 'Object]
               [prototype prototype])))

(define Object:prototype
  (new Object% [prototype es-null]))

(define object-constructor
  (make-native-function
   (λ ([value ecma:undefined])
     (cond
       [(Object? value) value]
       [(or (string? value)
            (boolean? value)
            (number? value))
        (to-object value)]
       [else (new Object%)]))))

(define (from-property-descriptor desc)
  (if desc
      (let ([obj (new Object%)])
        (if (data-property? desc)
            (begin
              (define-own-property obj "value"
                    `(data (value . ,(data-property-value desc))
                           (writable . #t)
                           (enumerable . #t)
                           (configurable . #t))
                    #f)
              (define-own-property obj "writable"
                    `(data (value . ,(data-property-writable? desc))
                           (writable . #t)
                           (enumerable . #t)
                           (configurable . #t))
                    #f))
            (begin
              (define-own-property obj "get"
                    `(data (value . ,(accessor-property-get desc))
                           (writable . #t)
                           (enumerable . #t)
                           (configurable . #t))
                    #f)
              (define-own-property obj "set"
                    `(data (value . ,(accessor-property-set desc))
                           (writable . #t)
                           (enumerable . #t)
                           (configurable . #t))
                    #f)))
        (define-own-property obj "enumerable"
              `(data (value . ,(property-enumerable? desc))
                     (writable . #t)
                     (enumerable . #t)
                     (configurable . #t))
              #f)
        (define-own-property obj "configurable"
              `(data (value . ,(property-configurable? desc))
                     (writable . #t)
                     (enumerable . #t)
                     (configurable . #t))
              #f)
        obj)
      ecma:undefined))

(define (to-property-descriptor obj)
  (unless (Object? obj)
    (raise-native-error 'type "not an object"))
  (let ([oprops (get-field properties obj)])
    (define-values (enumerable? configurable?
                    value writable? get set)
      (apply
       values
       (map
        (λ (name)
          (hash-ref oprops name ecma:undefined))
        '("enumerable" "configurable"
          "value" "writable" "get" "set"))))
    (define-values (kind attrs)
      (cond
        [(or (not (ecma:undefined? value))
             (not (ecma:undefined? writable?)))
         (values 'data
                 (append
                  (if (ecma:undefined? value) '() `((value . ,value)))
                  (if (ecma:undefined? writable?) '() `((writable . ,writable?)))))]
        [else
         (values 'accessor
                 (append
                  (if (ecma:undefined? get) '() `((get . ,get)))
                  (if (ecma:undefined? set) '() `((set . ,set)))))]))
    (cons kind
          (append attrs
                  (if (ecma:undefined? enumerable?) '() `((enumerable . ,enumerable?)))
                  (if (ecma:undefined? configurable?) '() `((configurable . ,configurable?)))))))

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
      (get-own-property o (to-string p))))]
  ["getOwnPropertyNames"
   (native-method (o)
     (check-is-object o)
     (apply
      make-array
      (hash-keys (get-field properties o))))]
  ["create"
   (native-method (o properties)
     (unless (or (ecma:null? o) (Object? o))
       (raise-native-error 'type "not an object or null"))
     (let ([obj (new Object% [prototype (if (ecma:null? o) #f o)])])
       (unless (ecma:undefined? properties)
         (for ([(pname pdesc) (in-hash
                               (get-field properties
                                (to-object properties)))]
               #:when (property-enumerable? pdesc))
           (define-own-property obj pname
                 (to-property-descriptor
                  (get-property-value obj pname))
                 #t)))
       obj))]
  ["defineProperty"
   (native-method (o p attributes)
     (check-is-object o)
     (define-own-property o (to-string p)
           (to-property-descriptor attributes)
           #t)
     o)]
  ["defineProperties"
   (native-method (o properties)
     (check-is-object o)
     (for ([(pname pdesc) (in-hash
                           (get-field properties
                            (to-object properties)))]
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
      make-array
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
        [(ecma:undefined? ecma:this) "[object Undefined]"]
        [(ecma:null? ecma:this) "[object Null]"]
        [(format "[object ~a]"
                 (get-field class-name (to-object ecma:this)))])))]
  ["toLocaleString"
   (make-native-function
    (λ ()
      (define o (to-object ecma:this))
      (define f (get-property-value o "toString"))
      (unless (Function? f)
        (raise-native-error 'type "toString: not a function"))
      (apply/this f o '())))]
  ["valueOf"
   (make-native-function
    (λ ()
      (to-object ecma:this)))]
  ["hasOwnProperty"
   (make-native-function
    (λ (v)
      (property?
       (get-own-property
        (to-object ecma:this)
        (to-string v)))))]
  ["isPrototypeOf"
   (make-native-function
    (λ (v)
      (and (Object? v)
           (let ([o (to-object ecma:this)])
             (let loop ([v (get-field prototype v)])
               (and (not (ecma:null? v))
                    (or (eq? o v)
                        (loop (get-field prototype v)))))))))]
  ["propertyIsEnumerable"
   (make-native-function
    (λ (v)
      (define prop
        (get-own-property
         (to-object ecma:this)
         (to-string v)))
      (and (property? prop)
           (property-enumerable? prop))))])
