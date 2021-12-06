#lang typed/racket/base

(module definitions typed/racket/base
(require typed/racket/class
         "initializable.rkt"
         "primitive.rkt")

(provide (all-defined-out))

(struct es-property
  ([enumerable? : (Initializable Boolean)]
   [configurable? : (Initializable Boolean)])
  #:type-name ESProperty
  #:mutable
  #:transparent)

(struct data-property es-property
  ([value : (Initializable (Boxof ESValue))]
   [writable? : (Initializable Boolean)])
  #:type-name ESDataProperty
  #:mutable
  #:transparent)

(define (make-data-property [value : (Initializable ESValue) uninitialized]
                            #:writable? [writable? : (Initializable Boolean) uninitialized]
                            #:enumerable? [enumerable? : (Initializable Boolean) uninitialized]
                            #:configurable? [configurable? : (Initializable Boolean) uninitialized])
  (data-property enumerable? configurable? (if (initialized? value) (box value) uninitialized) writable?))

(struct accessor-property es-property
  ([get : (Initializable (U ESUndefined (-> ESValue ESValue)))]
   [set : (Initializable (U ESUndefined (-> ESValue ESValue Void)))])
  #:type-name ESAccessorProperty
  #:mutable
  #:transparent)

(define (make-accessor-property #:get [get : (Initializable (U ESUndefined (-> ESValue ESValue))) uninitialized]
                                #:set [set : (Initializable (U ESUndefined (-> ESValue ESValue Void))) uninitialized]
                                #:enumerable? [enumerable? : (Initializable Boolean) uninitialized]
                                #:configurable? [configurable? : (Initializable Boolean) uninitialized])
  (accessor-property enumerable? configurable? get set))

(define-type ESPropertyKey (U ESSymbol ESString))

(define-type ESObject<%>
  (Class (init-field [class-name Symbol]
                     [prototype (U ESObject ESNull)]
                     [extensible? Boolean #:optional]
                     [properties (Mutable-HashTable ESPropertyKey (U ESDataProperty ESAccessorProperty)) #:optional])
         [clone (-> ESObject)]
         [get-prototype (-> (U ESObject ESNull))]
         [set-prototype! (-> (U ESObject ESNull) Boolean)]
         [is-extensible? (-> Boolean)]
         [prevent-extensions! (-> Boolean)]
         [get-own-property (-> ESPropertyKey (U ESUndefined ESDataProperty ESAccessorProperty))]
         [define-own-property! (-> ESPropertyKey ESProperty Boolean)]
         [has-property? (-> ESPropertyKey Boolean)]
         [get-property (-> ESPropertyKey ESValue ESValue)]
         [set-property! (-> ESPropertyKey ESValue ESValue Boolean)]
         [delete-property! (-> ESPropertyKey Boolean)]
         [own-property-keys (-> (Listof ESPropertyKey))]))

(define-type ESObject (Instance ESObject<%>))

(define-type ESValue (U ESPrimitive ESObject))

(: is-compatible-property-descriptor? (-> Boolean ESProperty (U ESUndefined ESDataProperty ESAccessorProperty) Boolean))
(: validate-and-apply-property-descriptor!
   (case->
    (-> ESUndefined ESUndefined Boolean ESProperty (U ESUndefined ESDataProperty ESAccessorProperty) Boolean)
    (-> ESObject ESPropertyKey Boolean ESProperty (U ESUndefined ESDataProperty ESAccessorProperty) Boolean)))

(define es-object% : ESObject<%>
  (class object%
    (init-field class-name
                prototype
                [extensible? #t]
                [properties (make-hash)])
    (super-new)
    (define/public (clone)
      ; FIXME: cache object graph
      (new es-object%
           [class-name class-name]
           [prototype prototype] ; FIXME
           [properties (hash-copy properties)]
           [extensible? extensible?]))
    (define/public (get-prototype)
      prototype)
    (define/public (set-prototype! v)
      (cond
        [(equal? v prototype) #t]
        [(not extensible?) #f]
        [else
         (let loop ([p v])
           (cond
             [(es-null? p)
              (set! prototype v)
              #t]
             [(equal? v this) #f]
             [else (loop (get-field prototype p))]))]))
    (define/public (is-extensible?)
      extensible?)
    (define/public (prevent-extensions!)
      (set! extensible? #f)
      #t)
    (define/public (get-own-property p)
      (hash-ref properties p (λ () es-undefined)))
    (define/public (define-own-property! p desc)
      (validate-and-apply-property-descriptor! this p extensible? desc (get-own-property p)))
    (define/public (has-property? p)
      (or (not (es-null? (get-own-property p)))
          (let ([parent (get-prototype)])
            (and (not (es-null? parent))
                 (send parent has-property? p)))))
    (define/public (get-property p receiver)
      (let ([desc (get-own-property p)])
        (cond
          [(es-undefined? desc)
           (let ([parent (get-prototype)])
             (if (es-null? parent)
                 es-undefined
                 (send parent get-property p receiver)))]
          [(data-property? desc)
           (let ([value (data-property-value desc)])
             (assert value initialized?)
             (unbox value))]
          [else
           (let ([getter (accessor-property-get desc)])
             (assert getter initialized?)
             (if (es-undefined? getter)
                 es-undefined
                 (getter receiver)))])))
    (define/public (set-property! p v receiver)
      (let loop ([desc (get-own-property p)])
        (cond
          [(es-undefined? desc)
           (let ([parent (get-prototype)])
             (if (es-null? parent)
                 (loop (make-data-property es-undefined
                                           #:writable? #t
                                           #:enumerable? #t
                                           #:configurable? #t))
                 (send parent set-property! p v receiver)))]
          [(data-property? desc)
           (and (data-property-writable? desc)
                (is-a? receiver es-object%)
                (let* ([receiver (cast receiver ESObject)]
                       [existing (send receiver get-own-property p)])
                  (if (es-undefined? existing)
                      (send receiver define-own-property! p
                            (make-data-property v
                                                #:writable? #t
                                                #:enumerable? #t
                                                #:configurable? #t))
                      (and (not (accessor-property? existing))
                           (data-property-writable? existing)
                           (send receiver define-own-property! p
                                 (make-data-property v))))))]
          [else
           (let ([setter (accessor-property-set desc)])
             (assert setter initialized?)
             (if (es-undefined? setter)
                 #f
                 (begin
                   (setter receiver v)
                   #t)))])))
    (define/public (delete-property! p)
      (let ([desc (get-own-property p)])
        (cond
          [(es-undefined? desc) #t]
          [(es-property-configurable? desc)
           (hash-remove! properties p)
           #t]
          [else #f])))
    (define/public (own-property-keys)
      ; FIXME: order
      (hash-keys properties))))

(define (is-compatible-property-descriptor? extensible? desc current)
  (validate-and-apply-property-descriptor! es-undefined es-undefined extensible? desc current))

(define (validate-and-apply-property-descriptor! o p extensible? desc current)
  (define #:∀ (T) (coalesce [v : (Initializable T)] [default : T]) : T
    (if (uninitialized? v) default v))
  (define (update-property!)
    (unless (es-undefined? o)
      (let ([x (hash-ref (get-field properties o) p)])
        (let ([it (es-property-enumerable? desc)])
          (when (initialized? it) (set-es-property-enumerable?! x it)))
        (let ([it (es-property-configurable? desc)])
          (when (initialized? it) (set-es-property-configurable?! x it)))
        (when (and (data-property? x) (data-property? desc))
          (let ([it (data-property-value desc)])
            (when (initialized? it) (set-data-property-value! x it)))
          (let ([it (data-property-writable? desc)])
            (when (initialized? it) (set-data-property-writable?! x it))))
        (when (and (accessor-property? x) (accessor-property? desc))
          (let ([it (accessor-property-get desc)])
            (when (initialized? it) (set-accessor-property-get! x it)))
          (let ([it (accessor-property-set desc)])
            (when (initialized? it) (set-accessor-property-set! x it))))))
    #t)
  (cond
    [(es-undefined? current)
     (and
      extensible?
      (begin
        (unless (es-undefined? o)
          (let ([enumerable? (coalesce (es-property-enumerable? desc) #f)]
                [configurable? (coalesce (es-property-configurable? desc) #f)])
            (hash-set! (get-field properties o)
                       p
                       (if (accessor-property? desc)
                           (accessor-property
                            enumerable?
                            configurable?
                            (coalesce (accessor-property-get desc) es-undefined)
                            (coalesce (accessor-property-set desc) es-undefined))
                           (let-values ([(value writable?)
                                         (if (data-property? desc)
                                             (values (coalesce (data-property-value desc)
                                                               (ann (box es-undefined) (Boxof ESValue)))
                                                     (coalesce (data-property-writable? desc) #f))
                                             (values (ann (box es-undefined) (Boxof ESValue))
                                                     #f))])
                             (data-property
                              enumerable?
                              configurable?
                              value
                              writable?))))))
        #t))]
    [(and (not (es-property-configurable? current))
          (eq? #t (es-property-configurable? desc))
          (or (uninitialized? (es-property-enumerable? desc))
              (not (equal? (es-property-enumerable? desc)
                           (es-property-enumerable? current)))))
     #f]
    [(and (not (data-property? desc)) (not (accessor-property? desc)))
     (update-property!)]
    [(not (eq? (data-property? current) (data-property? desc)))
     (and (es-property-configurable? current)
          (begin
            (unless (es-undefined? o)
              (hash-set! (get-field properties o)
                         p
                         (if (data-property? current)
                             (accessor-property (es-property-enumerable? current)
                                                (es-property-configurable? current)
                                                es-undefined
                                                es-undefined)
                             (data-property (es-property-enumerable? current)
                                            (es-property-configurable? current)
                                            (box es-undefined)
                                            #f))))
            (update-property!)))]
    [(and (data-property? current) (data-property? desc))
     (if (or (es-property-configurable? current)
             (data-property-writable? current))
         (update-property!)
         (and (not (eq? #t (data-property-writable? current)))
              (equal? (data-property-value desc)
                      (data-property-value current))))]
    [else
     (assert (accessor-property? current))
     (assert (accessor-property? desc))
     (if (es-property-configurable? current)
         (update-property!)
         (and (equal? (accessor-property-set desc)
                      (accessor-property-set current))
              (equal? (accessor-property-get desc)
                      (accessor-property-get current))))]))
)

(module predicate racket/base
  (require racket/class
           (submod ".." definitions))
  (provide es-object?)
  (define (es-object? v)
    (is-a? v es-object%)))

(require typed/racket/unsafe
         (submod "." definitions))
;; This really is unsafe: untyped code can set fields, which will not be caught by is-a?.
(unsafe-require/typed
 (submod "." predicate)
 [es-object? (-> Any Boolean : ESObject)])
(provide (all-from-out (submod "." definitions))
         es-object?
         (all-defined-out))

(require typed/racket/class
         "primitive.rkt"
         "string.rkt")

(require/typed "error.rkt" ; TODO
 [raise-native-error (->* (Symbol) (String) Nothing)])

(: es-value? (-> Any Boolean : ESValue))
(define (es-value? v)
  (or (es-primitive? v)
      (es-object? v)))

(define (extensible? [o : ESObject])
  (get-field extensible? o))

(define (get-property [o : ESObject] [p : ESPropertyKey])
  (send o get-property p o))

(define (set-property! [o : ESObject] [p : ESPropertyKey] [v : ESValue] [throw? : Boolean])
  (or (send o set-property! p v o)
      (begin
        (when throw? (raise-native-error 'type))
        #f)))

(define (define-property-or-throw! [o : ESObject] [p : ESPropertyKey] [desc : ESProperty])
  (or (send o define-own-property! p desc)
      (raise-native-error 'type)))

(define (has-property? [o : ESObject] [p : ESPropertyKey])
  (send o has-property? p))

(define (has-own-property? [o : ESObject] [p : ESPropertyKey])
  (not (es-undefined? (send o get-own-property p))))

; TODO: update (and remove compat stuff)

(define (get-property-descriptor [o : ESObject] [p : String])
  (define receiver o)
  (let loop : (U ESUndefined ESDataProperty ESAccessorProperty)
            ([o : ESObject o])
    (let ([desc (send o get-own-property (string->es-string p))])
      (if (es-undefined? desc)
          (let ([parent (send o get-prototype)])
            (if (es-null? parent)
                es-undefined
                (loop parent)))
          desc))))

(define (get-own-property [o : ESObject] [p : String])
  (send o get-own-property (string->es-string p)))

(define (get-property-value [o : ESObject] [p : String])
  (send o get-property (string->es-string p) o))

(define (can-set-property? [o : ESObject] [p : String])
  (define (property-settable? desc)
    (cond
      [(accessor-property? desc) (not (es-undefined? (accessor-property-set desc)))]
      [(data-property? desc) (data-property-writable? desc)]
      [else #f]))
  (let ([desc (get-own-property o p)]
        [prototype (send o get-prototype)])
    (cast
     (cond
       [(property-settable? desc)]
       [(es-null? prototype) (get-field extensible? o)]
       [else
        (let ([desc (get-property-descriptor prototype p)])
          (or (property-settable? desc)
             (get-field extensible? o)))])
     Boolean)))

(define (set-property-value! [o : ESObject] [p : String] [v : ESValue] [throw? : Boolean #t])
  (cond
    [(send o set-property! (string->es-string p) v o) (void)]
    [throw? (raise-native-error 'type (format "~a: can't set property" p))]
    [else (void)]))

(define (delete-property! [o : ESObject] [p : String] [throw? : Boolean #t])
  (cond
    [(send o delete-property! (string->es-string p)) #t]
    [throw? (raise-native-error 'type (format "~a: not configurable" p))]
    [else #f]))

; TODO: proper object definition syntax

(define-type CompatPropertyDescriptor
  (U (Pairof
      'data
      (Listof
       (U (Pairof 'configurable Boolean)
          (Pairof 'enumerable Boolean)
          (Pairof 'writable Boolean)
          (Pairof 'value ESValue))))
     (Pairof
      'accessor
      (Listof
       (U (Pairof 'configurable Boolean)
          (Pairof 'enumerable Boolean)
          (Pairof 'get (-> ESValue))
          (Pairof 'set (-> ESValue Any)))))))

(define (define-own-property [o : ESObject] [p : String] [desc : CompatPropertyDescriptor] [throw? : Boolean])
  (define x
    (case (car desc)
      [(data)
       (let ([x (make-data-property)])
         (for ([f (cdr desc)])
           (case (car f)
             [(configurable) (set-es-property-configurable?! x (cdr f))]
             [(enumerable) (set-es-property-enumerable?! x (cdr f))]
             [(writable) (set-data-property-writable?! x (cdr f))]
             [(value) (set-data-property-value! x (box (cdr f)))]))
         x)]
      [(accessor)
       (let ([x (make-accessor-property)])
         (for ([f (cdr desc)])
           (case (car f)
             [(configurable) (set-es-property-configurable?! x (cdr f))]
             [(enumerable) (set-es-property-enumerable?! x (cdr f))]
             [(get) (set-accessor-property-get! x (λ (receiver) ((cdr f))))]
             [(set) (set-accessor-property-set! x (λ ([v : ESValue] receiver) (void ((cdr f) v))))]))
         x)]))
  (cond
    [(send o define-own-property! (string->es-string p) x) (void)]
    [throw? (raise-native-error 'type)]
    [else (void)]))

(define Object? es-object?)

(module compat-untyped racket/base
(require racket/class
         "string.rkt"
         (submod ".." definitions))
(provide define-object-properties
         (rename-out
          [es-object% ecma-object%]
          [es-property? property?]
          [es-property-enumerable? property-enumerable?]
          [set-es-property-enumerable?! set-property-enumerable?!]
          [es-property-configurable? property-configurable?]
          [set-es-property-configurable?! set-property-configurable?!]))

(define (compat:->es-value v)
  (if (string? v)
      (string->es-string v)
      v))

(define-syntax-rule (define-object-properties obj [prop val] ...)
  (begin
    (hash-set! (get-field properties obj)
               (string->es-string prop)
               (make-data-property (compat:->es-value val)
                                   #:writable? #t
                                   #:enumerable? #f
                                   #:configurable? #t)) ...))
)
(require (submod "." compat-untyped))
(provide (all-from-out (submod "." compat-untyped)))
