#lang typed/racket/base

(module definitions typed/racket/base
(require typed/racket/class
         "primitive.rkt")

(provide (all-defined-out))

(struct es-property
  ([enumerable? : (U Void Boolean)]
   [configurable? : (U Void Boolean)])
  #:type-name ESProperty
  #:mutable
  #:transparent)

(struct es-data-property es-property
  ([value : (U Void ESValue)]
   [writable? : (U Void Boolean)])
  #:type-name ESDataProperty
  #:mutable
  #:transparent)

(struct es-accessor-property es-property
  ([get : (U Void ESUndefined (-> ESValue ESValue))]
   [set : (U Void ESUndefined (-> ESValue ESValue Void))])
  #:type-name ESAccessorProperty
  #:mutable
  #:transparent)

(define-type ESPropertyKey (U ESSymbol ESString))

(define-type ESObject<%>
  (Class (init-field [class-name Symbol]
                     [prototype (U ESObject ESNull)]
                     [extensible? Boolean #:optional]
                     [properties (Mutable-HashTable ESPropertyKey (U ESDataProperty ESAccessorProperty)) #:optional])
         [clone (-> Any)]
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
          [(es-data-property? desc)
           (let ([value (es-data-property-value desc)])
             (assert (not (void? value)))
             value)]
          [else
           (let ([getter (es-accessor-property-get desc)])
             (assert (not (void? getter)))
             (if (es-undefined? getter)
                 es-undefined
                 (getter receiver)))])))
    (define/public (set-property! p v receiver)
      (let loop ([desc (get-own-property p)])
        (cond
          [(es-undefined? desc)
           (let ([parent (get-prototype)])
             (if (es-null? parent)
                 (loop (es-data-property #t #t es-undefined #t))
                 (send parent set-property! p v receiver)))]
          [(es-data-property? desc)
           (and (es-data-property-writable? desc)
                (is-a? receiver es-object%)
                (let* ([receiver (cast receiver ESObject)]
                       [existing (send receiver get-own-property p)])
                  (if (es-undefined? existing)
                      (send receiver define-own-property! p (es-data-property #t #t v #t))
                      (and (not (es-accessor-property? existing))
                           (es-data-property-writable? existing)
                           (send receiver define-own-property! p (es-data-property (void) (void) v (void)))))))]
          [else
           (let ([setter (es-accessor-property-set desc)])
             (assert (not (void? setter)))
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
  (define #:∀ (T) (coalesce [v : (U Void T)] [default : T]) : T
    (if (void? v) default v))
  (define (update-property!)
    (unless (es-undefined? o)
      (let ([x (hash-ref (get-field properties o) p)])
        (let ([it (es-property-enumerable? desc)])
          (unless (void? it) (set-es-property-enumerable?! x it)))
        (let ([it (es-property-configurable? desc)])
          (unless (void? it) (set-es-property-configurable?! x it)))
        (when (and (es-data-property? x) (es-data-property? desc))
          (let ([it (es-data-property-value desc)])
            (unless (void? it) (set-es-data-property-value! x it)))
          (let ([it (es-data-property-writable? desc)])
            (unless (void? it) (set-es-data-property-writable?! x it))))
        (when (and (es-accessor-property? x) (es-accessor-property? desc))
          (let ([it (es-accessor-property-get desc)])
            (unless (void? it) (set-es-accessor-property-get! x it)))
          (let ([it (es-accessor-property-set desc)])
            (unless (void? it) (set-es-accessor-property-set! x it))))))
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
                       (if (es-accessor-property? desc)
                           (es-accessor-property
                            enumerable?
                            configurable?
                            (coalesce (es-accessor-property-get desc) es-undefined)
                            (coalesce (es-accessor-property-set desc) es-undefined))
                           (let-values ([(value writable?)
                                         (if (es-data-property? desc)
                                             (values (coalesce (es-data-property-value desc) es-undefined)
                                                     (coalesce (es-data-property-writable? desc) #f))
                                             (values es-undefined #f))])
                             (es-data-property
                              enumerable?
                              configurable?
                              value
                              writable?))))))
        #t))]
    [(and (not (es-property-configurable? current))
          (eq? #t (es-property-configurable? desc))
          (or (void? (es-property-enumerable? desc))
              (not (equal? (es-property-enumerable? desc)
                           (es-property-enumerable? current)))))
     #f]
    [(and (not (es-data-property? desc)) (not (es-accessor-property? desc)))
     (update-property!)]
    [(not (eq? (es-data-property? current) (es-data-property? desc)))
     (and (es-property-configurable? current)
          (begin
            (unless (es-undefined? o)
              (hash-set! (get-field properties o)
                         p
                         (if (es-data-property? current)
                             (es-accessor-property (es-property-enumerable? current)
                                                   (es-property-configurable? current)
                                                   es-undefined
                                                   es-undefined)
                             (es-data-property (es-property-enumerable? current)
                                               (es-property-configurable? current)
                                               es-undefined
                                               #f))))
            (update-property!)))]
    [(and (es-data-property? current) (es-data-property? desc))
     (if (or (es-property-configurable? current)
             (es-data-property-writable? current))
         (update-property!)
         (and (not (eq? #t (es-data-property-writable? current)))
              (equal? (es-data-property-value desc)
                      (es-data-property-value current))))]
    [else
     (assert (es-accessor-property? current))
     (assert (es-accessor-property? desc))
     (if (es-property-configurable? current)
         (update-property!)
         (and (equal? (es-accessor-property-set desc)
                      (es-accessor-property-set current))
              (equal? (es-accessor-property-get desc)
                      (es-accessor-property-get current))))]))
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

; TODO: update (and remove compat stuff)

(require typed/racket/class
         "primitive.rkt"
         "string.rkt")

(require/typed "error.rkt" ; TODO
 [raise-native-error (->* (Symbol) (String) Nothing)])

(define (get-property [o : ESObject] [p : String])
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
      [(es-accessor-property? desc) (not (es-undefined? (es-accessor-property-set desc)))]
      [(es-data-property? desc) (es-data-property-writable? desc)]
      [else #f]))
  (let ([desc (get-own-property o p)]
        [prototype (send o get-prototype)])
    (cast
     (cond
       [(property-settable? desc)]
       [(es-null? prototype) (get-field extensible? o)]
       [else
        (let ([desc (get-property prototype p)])
          (or (property-settable? desc)
             (get-field extensible? o)))])
     Boolean)))

(define (set-property-value! [o : ESObject] [p : String] [v : ESValue] [throw? : Boolean #t])
  (cond
    [(send o set-property! (string->es-string p) v o) (void)]
    [throw? (raise-native-error 'type (format "~a: can't set property" p))]
    [else (void)]))

(define (has-property? [o : ESObject] [p : String])
  (send o has-property? (string->es-string p)))

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
       (let ([x (es-data-property (void) (void) (void) (void))])
         (for ([f (cdr desc)])
           (case (car f)
             [(configurable) (set-es-property-configurable?! x (cdr f))]
             [(enumerable) (set-es-property-enumerable?! x (cdr f))]
             [(writable) (set-es-data-property-writable?! x (cdr f))]
             [(value) (set-es-data-property-value! x (cdr f))]))
         x)]
      [(accessor)
       (let ([x (es-accessor-property (void) (void) (void) (void))])
         (for ([f (cdr desc)])
           (case (car f)
             [(configurable) (set-es-property-configurable?! x (cdr f))]
             [(enumerable) (set-es-property-enumerable?! x (cdr f))]
             [(get) (set-es-accessor-property-get! x (λ (receiver) ((cdr f))))]
             [(set) (set-es-accessor-property-set! x (λ ([v : ESValue] receiver) (void ((cdr f) v))))]))
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
          [set-es-property-configurable?! set-property-configurable?!]
          [es-data-property? data-property?]
          [es-data-property-value data-property-value]
          [set-es-data-property-value! set-data-property-value!]
          [es-data-property-writable? data-property-writable?]
          [set-es-data-property-writable?! set-data-property-writable?!]
          [es-accessor-property? accessor-property?]
          [es-accessor-property-get accessor-property-get]
          [set-es-accessor-property-get! set-accessor-property-get!]
          [es-accessor-property-set accessor-property-set]
          [set-es-accessor-property-set! accessor-property-set!]))

(define (compat:->es-value v)
  (if (string? v)
      (string->es-string v)
      v))

(define-syntax-rule (define-object-properties obj [prop val] ...)
  (begin
    (hash-set! (get-field properties obj)
               (string->es-string prop)
               (es-data-property #f #t (compat:->es-value val) #t)) ...))
)
(require (submod "." compat-untyped))
(provide (all-from-out (submod "." compat-untyped)))
