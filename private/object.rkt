#lang typed/racket/base

(require typed/racket/class
         "initializable.rkt"
         "primitive.rkt"
         "string.rkt"
         "unsafe-predicate.rkt")

(require/typed "error.rkt" ; TODO
 [raise-native-error (->* (Symbol) (String) Nothing)])

(require typed/racket/unsafe)
(module lazy racket/base
  (require racket/lazy-require)
  (lazy-require
   ["../convert.rkt" (to-object)])
  (provide to-object))
(unsafe-require/typed (submod "." lazy)
                      [to-object (-> Any ESObject)])

(provide (all-defined-out))

(struct property
  ([enumerable? : (Initializable Boolean)]
   [configurable? : (Initializable Boolean)])
  #:type-name ESProperty
  #:mutable
  #:transparent)

(struct data-property property
  ([value : (Initializable (Boxof Any))]
   [writable? : (Initializable Boolean)])
  #:type-name ESDataProperty
  #:mutable
  #:transparent)

(define (make-data-property [value : (Initializable Any) uninitialized] ; FIXME
                            #:writable? [writable? : (Initializable Boolean) uninitialized]
                            #:enumerable? [enumerable? : (Initializable Boolean) uninitialized]
                            #:configurable? [configurable? : (Initializable Boolean) uninitialized])
  (data-property enumerable? configurable? (if (initialized? value) (box value) uninitialized) writable?))

(struct accessor-property property
  ([get : (Initializable (U ESUndefined (-> Any Any)))]
   [set : (Initializable (U ESUndefined (-> Any Any Void)))])
  #:type-name ESAccessorProperty
  #:mutable
  #:transparent)

(define (make-accessor-property #:get [get : (Initializable (U ESUndefined (-> Any Any))) uninitialized]
                                #:set [set : (Initializable (U ESUndefined (-> Any Any Void))) uninitialized]
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
         [get (-> ESPropertyKey Any Any)]
         [set! (-> ESPropertyKey Any Any Boolean)]
         [delete! (-> ESPropertyKey Boolean)]
         [own-property-keys (-> (Listof ESPropertyKey))]))

(define-type ESObject (Instance ESObject<%>))

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
              (set-field! prototype this v)
              #t]
             [(equal? v this) #f]
             [else (loop (get-field prototype p))]))]))
    (define/public (is-extensible?)
      extensible?)
    (define/public (prevent-extensions!)
      (set-field! extensible? this #f)
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
    (define/public (get p receiver)
      (let ([desc (get-own-property p)])
        (cond
          [(es-undefined? desc)
           (let ([parent (get-prototype)])
             (if (es-null? parent)
                 es-undefined
                 (send parent get p receiver)))]
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
    (define/public (set! p v receiver)
      (let loop ([desc (get-own-property p)])
        (cond
          [(es-undefined? desc)
           (let ([parent (get-prototype)])
             (if (es-null? parent)
                 (loop (make-data-property es-undefined
                                           #:writable? #t
                                           #:enumerable? #t
                                           #:configurable? #t))
                 (send parent set! p v receiver)))]
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
    (define/public (delete! p)
      (let ([desc (get-own-property p)])
        (cond
          [(es-undefined? desc) #t]
          [(property-configurable? desc)
           (hash-remove! properties p)
           #t]
          [else #f])))
    (define/public (own-property-keys)
      ; FIXME: order
      (hash-keys properties))))

(define-unsafe-class-predicate es-object? es-object% ESObject<%>)

(define (is-compatible-property-descriptor? extensible? desc current)
  (validate-and-apply-property-descriptor! es-undefined es-undefined extensible? desc current))

(define (validate-and-apply-property-descriptor! o p extensible? desc current)
  (define #:∀ (T) (coalesce [v : (Initializable T)] [default : T]) : T
    (if (uninitialized? v) default v))
  (define (update-property!)
    (unless (es-undefined? o)
      (let ([x (hash-ref (get-field properties o) p)])
        (let ([it (property-enumerable? desc)])
          (when (initialized? it) (set-property-enumerable?! x it)))
        (let ([it (property-configurable? desc)])
          (when (initialized? it) (set-property-configurable?! x it)))
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
          (let ([enumerable? (coalesce (property-enumerable? desc) #f)]
                [configurable? (coalesce (property-configurable? desc) #f)])
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
                                                               (ann (box es-undefined) (Boxof Any)))
                                                     (coalesce (data-property-writable? desc) #f))
                                             (values (ann (box es-undefined) (Boxof Any))
                                                     #f))])
                             (data-property
                              enumerable?
                              configurable?
                              value
                              writable?))))))
        #t))]
    [(and (not (property-configurable? current))
          (eq? #t (property-configurable? desc))
          (or (uninitialized? (property-enumerable? desc))
              (not (equal? (property-enumerable? desc)
                           (property-enumerable? current)))))
     #f]
    [(and (not (data-property? desc)) (not (accessor-property? desc)))
     (update-property!)]
    [(not (eq? (data-property? current) (data-property? desc)))
     (and (property-configurable? current)
          (begin
            (unless (es-undefined? o)
              (hash-set! (get-field properties o)
                         p
                         (if (data-property? current)
                             (accessor-property (property-enumerable? current)
                                                (property-configurable? current)
                                                es-undefined
                                                es-undefined)
                             (data-property (property-enumerable? current)
                                            (property-configurable? current)
                                            (box es-undefined)
                                            #f))))
            (update-property!)))]
    [(and (data-property? current) (data-property? desc))
     (if (or (property-configurable? current)
             (data-property-writable? current))
         (update-property!)
         (and (not (eq? #t (data-property-writable? current)))
              (equal? (data-property-value desc)
                      (data-property-value current))))]
    [else
     (assert (accessor-property? current))
     (assert (accessor-property? desc))
     (if (property-configurable? current)
         (update-property!)
         (and (equal? (accessor-property-set desc)
                      (accessor-property-set current))
              (equal? (accessor-property-get desc)
                      (accessor-property-get current))))]))

(define (extensible? [o : ESObject])
  (get-field extensible? o))

(define (get/object [o : ESObject] [p : ESPropertyKey])
  (send o get p o))

(define (get/value [v : Any] [p : ESPropertyKey])
  (send (to-object v) get p v))

(define (set!/object [o : ESObject] [p : ESPropertyKey] [v : Any] [throw? : Boolean])
  (or (send o set! p v o)
      (begin
        (when throw? (raise-native-error 'type))
        #f)))

(define (create-data-property! [o : ESObject] [p : ESPropertyKey] [v : Any])
  (send o define-own-property!
        p
        (make-data-property v
                            #:writable? #t
                            #:enumerable? #t
                            #:configurable? #t)))

(define (create-method-property! [o : ESObject] [p : ESPropertyKey] [v : Any])
  (send o define-own-property!
        p
        (make-data-property v
                            #:writable? #t
                            #:enumerable? #f
                            #:configurable? #t)))

(define (create-data-property-or-throw! [o : ESObject] [p : ESPropertyKey] [v : Any])
  (unless (create-data-property! o p v)
    (raise-native-error 'type)))

(define (define-property-or-throw! [o : ESObject] [p : ESPropertyKey] [desc : ESProperty])
  (unless (send o define-own-property! p desc)
    (raise-native-error 'type)))

(define (delete-property-or-throw! [o : ESObject] [p : ESPropertyKey])
  (unless (send o delete! p)
    (raise-native-error 'type)))

(define (has-property? [o : ESObject] [p : ESPropertyKey])
  (send o has-property? p))

(define (has-own-property? [o : ESObject] [p : ESPropertyKey])
  (not (es-undefined? (send o get-own-property p))))

(define (set-integrity-level! [o : ESObject] [level : (U 'sealed 'frozen)])
  (and (send o prevent-extensions!)
       (let ([keys (send o own-property-keys)])
         (if (eq? 'sealed level)
             (for ([k (in-list keys)])
               (define-property-or-throw! o k (property uninitialized #f)))
             (for ([k (in-list keys)])
               (let ([current (send o get-own-property k)])
                 (unless (es-undefined? current)
                   (define-property-or-throw!
                     o k
                     (if (accessor-property? current)
                         (property uninitialized #f)
                         (data-property uninitialized #f uninitialized #f)))))))
         #t)))

(define (test-integrity-level [o : ESObject] [level : (U 'sealed 'frozen)])
  (and (not (extensible? o))
       (for/and : Boolean ([k (in-list (send o own-property-keys))])
         (let ([current (send o get-own-property k)])
           (cond
             [(es-undefined? current) #t]
             [(property-configurable? current) #f]
             [(and (eq? 'frozen level) (data-property? current))
              (not (data-property-writable? current))]
             [else #t])))))

(define (copy-data-properties [target : ESObject] [source : Any] [excluded : (Listof ESPropertyKey)])
  (unless (or (es-undefined? source) (es-null? source))
    (let* ([from (to-object source)]
           [keys (send from own-property-keys)])
      (for ([key (in-list keys)]
            #:when (not (member key excluded)))
        (let ([desc (send from get-own-property key)])
          (when (and (not (es-undefined? desc))
                     (property-enumerable? desc))
            (create-data-property-or-throw! target key (get/object from key)))))))
  target)

; TODO: remove compat stuff

(define (get-own-property [o : ESObject] [p : String])
  (send o get-own-property (string->es-string p)))

(define (get-property-value [o : ESObject] [p : String])
  (send o get (string->es-string p) o))

(define (set-property-value! [o : ESObject] [p : String] [v : Any] [throw? : Boolean #t])
  (cond
    [(send o set! (string->es-string p) v o) (void)]
    [throw? (raise-native-error 'type (format "~a: can't set property" p))]
    [else (void)]))

(define (delete-property! [o : ESObject] [p : String] [throw? : Boolean #t])
  (cond
    [(send o delete! (string->es-string p)) #t]
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
          (Pairof 'value Any))))
     (Pairof
      'accessor
      (Listof
       (U (Pairof 'configurable Boolean)
          (Pairof 'enumerable Boolean)
          (Pairof 'get (-> Any))
          (Pairof 'set (-> Any Any)))))))

(define (define-own-property [o : ESObject] [p : String] [desc : CompatPropertyDescriptor] [throw? : Boolean])
  (define x
    (case (car desc)
      [(data)
       (let ([x (make-data-property)])
         (for ([f (cdr desc)])
           (case (car f)
             [(configurable) (set-property-configurable?! x (cdr f))]
             [(enumerable) (set-property-enumerable?! x (cdr f))]
             [(writable) (set-data-property-writable?! x (cdr f))]
             [(value) (set-data-property-value! x (box (cdr f)))]))
         x)]
      [(accessor)
       (let ([x (make-accessor-property)])
         (for ([f (cdr desc)])
           (case (car f)
             [(configurable) (set-property-configurable?! x (cdr f))]
             [(enumerable) (set-property-enumerable?! x (cdr f))]
             [(get) (set-accessor-property-get! x (λ (receiver) ((cdr f))))]
             [(set) (set-accessor-property-set! x (λ (v receiver) (void ((cdr f) v))))]))
         x)]))
  (cond
    [(send o define-own-property! (string->es-string p) x) (void)]
    [throw? (raise-native-error 'type)]
    [else (void)]))

(define Object? es-object?)
(define ecma-object% es-object%)

(module compat-untyped racket/base
(require racket/class
         racket/lazy-require
         "string.rkt")
(provide define-object-properties)
(lazy-require
 [(submod "..") (make-data-property)])

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
