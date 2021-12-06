#lang typed/racket/base

(module definitions typed/racket/base
(require typed/racket/class
         racket/set
         "../lang/helpers.rkt"
         "initializable.rkt"
         "object.rkt"
         "primitive.rkt"
         "string.rkt")

(require typed/racket/unsafe)
(unsafe-require/typed racket/set
                      [set-add! (∀ (T) (-> (Setof T) T Void))]
                      [set-remove! (∀ (T) (-> (Setof T) T Void))])

(require/typed "error.rkt" ; TODO
               [raise-native-error (->* (Symbol) ((U String ESString)) Nothing)])

(provide (all-defined-out))

(define-type ESEnvironment<%>
  (Class (init-field [outer-env (U ESNull ESEnvironment)])
         [has-binding? (-> ESString Boolean)]
         [create-mutable-binding! (-> ESString Boolean (U Void Boolean))]
         [create-immutable-binding! (-> ESString Boolean (U Void Boolean))]
         [initialize-binding! (-> ESString Any (U Void Boolean))]
         [set-mutable-binding! (-> ESString Any Boolean (U Void Boolean))]
         [get-binding-value (-> ESString Boolean Any)]
         [delete-binding! (-> ESString Boolean)]
         [has-this-binding? (-> Boolean)]
         [has-super-binding? (-> Boolean)]
         [with-base-object (-> (U ESObject ESUndefined))]))

(define-type ESEnvironment (Instance ESEnvironment<%>))

(struct binding
  ([value : (Initializable (Boxof Any))])
  #:type-name ESBinding
  #:mutable
  #:transparent)

(struct mutable-binding binding
  ([deletable? : Boolean])
  #:type-name ESMutableBinding
  #:transparent)

(struct immutable-binding binding
  ([strict? : Boolean])
  #:type-name ESImmutableBinding
  #:transparent)

(define-type ESDeclarativeEnvironment<%>
  (Class #:implements/inits ESEnvironment<%>))

(define-type ESDeclarativeEnvironment (Instance ESDeclarativeEnvironment<%>))

(define declarative-environment%
  : ESDeclarativeEnvironment<%>
  (class object%
    (init-field outer-env)
    (super-new)
    (define bindings : (Mutable-HashTable ESString (U ESMutableBinding ESImmutableBinding))
      (make-hash))
    (define/public (has-binding? n)
      (hash-has-key? bindings n))
    (define/public (create-mutable-binding! name deletable?)
      (assert (not (hash-has-key? bindings name)))
      (hash-set! bindings name (mutable-binding uninitialized deletable?)))
    (define/public (create-immutable-binding! name strict?)
      (assert (not (hash-has-key? bindings name)))
      (hash-set! bindings name (immutable-binding uninitialized strict?)))
    (define/public (initialize-binding! name v)
      (let ([b (hash-ref bindings name)])
        (assert (binding-value b) uninitialized?)
        (set-binding-value! b (box v))))
    (define/public (set-mutable-binding! name v strict?)
      (cond
        [(hash-ref bindings name (λ () #f))
         => (λ (b)
              (define old-v (binding-value b))
              (cond
                [(uninitialized? old-v)
                 (raise-native-error 'reference
                                     (es-string-append
                                      (es-string-literal "uninitialized: ")
                                      name))]
                [(mutable-binding? b)
                 (set-box! old-v v)]
                [else
                 (assert (immutable-binding? b))
                 (when (or strict? (immutable-binding-strict? b))
                   (raise-native-error 'type
                                       (es-string-append
                                        (es-string-literal "immutable: ")
                                        name)))]))]
        [else
         (when strict?
           (raise-native-error 'reference
                               (es-string-append
                                (es-string-literal "unbound: ")
                                name)))
         (create-mutable-binding! name #t)
         (initialize-binding! name v)]))
    (define/public (get-binding-value name strict?)
      (let* ([b (hash-ref bindings name)]
             [v (binding-value b)])
        (when (uninitialized? v)
          (raise-native-error 'reference
                              (es-string-append
                               (es-string-literal "uninitialized: ")
                               name)))
        (unbox v)))
    (define/public (delete-binding! name)
      (let ([b (hash-ref bindings name)])
        (and (mutable-binding? b)
             (mutable-binding-deletable? b)
             (begin
               (hash-remove! bindings name)
               #t)
             #f)))
    (define/public (has-this-binding?) #f)
    (define/public (has-super-binding?) #f)
    (define/public (with-base-object) es-undefined)))

(define-type ESObjectEnvironment<%>
  (Class #:implements/inits ESEnvironment<%>
         (init-field [binding-object ESObject]
                     [is-with-environment? Boolean])
         [clone (-> ESObjectEnvironment)]))

(define-type ESObjectEnvironment (Instance ESObjectEnvironment<%>))

(define object-environment%
  : ESObjectEnvironment<%>
  (class object%
    (init-field binding-object
                is-with-environment?
                outer-env)
    (super-new)
    (define/public (clone) ; TODO: remove
      (new object-environment%
           [outer-env (if (es-null? outer-env) outer-env (send (cast outer-env ESObjectEnvironment) clone))]
           [binding-object (if (es-object? binding-object) (send binding-object clone) binding-object)]
           [is-with-environment? is-with-environment?]))
    (define/public (has-binding? name)
      ; TODO: @@unscopables
      (or (send binding-object has-property? name)
          is-with-environment?))
    (define/public (create-mutable-binding! name deletable?)
      (define-property-or-throw!
        binding-object
        name
        (make-data-property es-undefined
                            #:writable? #t
                            #:enumerable? #t
                            #:configurable? deletable?)))
    (define/public (create-immutable-binding! name strict?)
      (error 'BUG))
    (define/public (initialize-binding! name v)
      (set-mutable-binding! name v #f))
    (define/public (set-mutable-binding! name v strict?)
      (unless (or (has-property? binding-object name)
                  (not strict?))
        (raise-native-error 'reference))
      (set-property! binding-object name v strict?))
    (define/public (get-binding-value name strict?)
      (cond
        [(has-property? binding-object name) (get-property binding-object name)]
        [strict? (raise-native-error 'reference)]
        [else es-undefined]))
    (define/public (delete-binding! name)
      (send binding-object delete-property! name))
    (define/public (has-this-binding?) #f)
    (define/public (has-super-binding?) #f)
    (define/public (with-base-object)
      (if is-with-environment?
          binding-object
          es-undefined))))

(define-type ESFunctionEnvironment<%>
  (Class #:implements/inits ESEnvironment<%>
         (init-field [this-value Any]
                     [this-binding-status (U 'lexical 'initialized 'uninitialized)]
                     [function-object ESObject]
                     [new-target (U ESObject ESUndefined)])
         [bind-this-value! (-> Any Any)]
         [get-this-binding (-> Any)]
         [get-super-base (-> (U Any ESUndefined))]))

(define-type ESFunctionEnvironment (Instance ESFunctionEnvironment<%>))

(define function-environment%
  : ESFunctionEnvironment<%>
  (class declarative-environment%
    (init-field this-value
                this-binding-status
                function-object
                new-target)
    (super-new)
    (define/public (bind-this-value! v)
      (assert (not (eq? 'lexical this-binding-status)))
      (when (eq? 'initialized this-binding-status)
        (raise-native-error 'reference))
      (set! this-value v)
      (set! this-binding-status 'initialized)
      v)
    (define/override (has-this-binding?)
      (not (eq? 'lexical this-binding-status)))
    (define/override (has-super-binding?)
      ; TODO: home-object
      (and (not (eq? 'lexical this-binding-status))))
    (define/public (get-this-binding)
      (assert (not (eq? 'lexical this-binding-status)))
      (when (eq? 'uninitialzied this-binding-status)
        (raise-native-error 'reference))
      this-value)
    (define/public (get-super-base)
      (error 'TODO))))

(define-type ESGlobalEnvironment<%>
  (Class #:implements/inits ESEnvironment<%>
         (init-field [object-record ESObjectEnvironment]
                     [global-this-value ESObject]
                     [declarative-record ESDeclarativeEnvironment]
                     [var-names (Setof ESString)])
         [get-this-binding (-> ESObject)]
         [has-var-declaration? (-> ESString Boolean)]
         [has-lexical-declaration? (-> ESString Boolean)]
         [has-restricted-global-property? (-> ESString Boolean)]
         [can-declare-global-var? (-> ESString Boolean)]
         [can-declare-global-function? (-> ESString Boolean)]
         [create-global-var-binding! (-> ESString Boolean Void)]
         [create-global-function-binding! (-> ESString Any Boolean Void)]))

(define-type ESGlobalEnvironment (Instance ESGlobalEnvironment<%>))

(define global-environment%
  : ESGlobalEnvironment<%>
  (class object%
    (init-field object-record
                global-this-value
                declarative-record
                var-names
                outer-env)
    (super-new)
    (define/public (has-binding? name)
      (or (send declarative-record has-binding? name)
          (send object-record has-binding? name)))
    (define/public (create-mutable-binding! name deletable?)
      (when (send declarative-record has-binding? name)
        (raise-native-error 'type-error))
      (send declarative-record create-mutable-binding! name deletable?))
    (define/public (create-immutable-binding! name strict?)
      (when (send declarative-record has-binding? name)
        (raise-native-error 'type-error))
      (send declarative-record create-immutable-binding! name strict?))
    (define/public (initialize-binding! name v)
      (if (send declarative-record has-binding? name)
          (send declarative-record initialize-binding! name v)
          (send object-record initialize-binding! name v)))
    (define/public (set-mutable-binding! name v strict?)
      (if (send declarative-record has-binding? name)
          (send declarative-record set-mutable-binding! name v strict?)
          (send object-record set-mutable-binding! name v strict?)))
    (define/public (get-binding-value name strict?)
      (if (send declarative-record has-binding? name)
          (send declarative-record get-binding-value name strict?)
          (send object-record get-binding-value name strict?)))
    (define/public (delete-binding! name)
      (if (send declarative-record has-binding? name)
          (send declarative-record delete-binding! name)
          (let ([obj (get-field binding-object object-record)])
            (if (has-own-property? obj name)
                (let ([status (send object-record delete-binding! name)])
                  (when status
                    (set-remove! var-names name))
                  status)
                #t))))
    (define/public (has-this-binding?) #t)
    (define/public (has-super-binding?) #f)
    (define/public (with-base-object) es-undefined)
    (define/public (get-this-binding) global-this-value)
    (define/public (has-var-declaration? name)
      (set-member? var-names name))
    (define/public (has-lexical-declaration? name)
      (send declarative-record has-binding? name))
    (define/public (has-restricted-global-property? name)
      (let* ([obj (get-field binding-object object-record)]
             [prop (send obj get-own-property name)])
        (and (not (es-undefined? prop))
             (not (property-configurable? prop)))))
    (define/public (can-declare-global-var? name)
      (let ([obj (get-field binding-object object-record)])
        (or (has-own-property? obj name)
            (extensible? obj))))
    (define/public (can-declare-global-function? name)
      (let* ([obj (get-field binding-object object-record)]
             [prop (send obj get-own-property name)])
        (cond
          [(es-undefined? prop) (extensible? obj)]
          [(cast (es-property-configurable? prop) Boolean)]
          [(and (data-property? prop)
                (cast (data-property-writable? prop) Boolean)
                (cast (es-property-enumerable? prop) Boolean))]
          [else #f])))
    (define/public (create-global-var-binding! name deletable?)
      (let ([obj (get-field binding-object object-record)])
        (when (and (not (has-own-property? obj name))
                   (extensible? obj))
          (send object-record create-mutable-binding! name deletable?)
          (send object-record initialize-binding! name es-undefined))
        (set-add! var-names name)))
    (define/public (create-global-function-binding! name v deletable?)
      (let* ([obj (get-field binding-object object-record)]
             [desc (let ([prop (send obj get-own-property name)])
                     (if (or (es-undefined? prop)
                             (es-property-configurable? prop))
                         (make-data-property v
                                             #:writable? #t
                                             #:enumerable? #t
                                             #:configurable? deletable?)
                         (make-data-property v)))])
        (define-property-or-throw! obj name desc)
        (set-property! obj name v #f)
        (set-add! var-names name)))))
)

(module predicates racket/base
  (require racket/class
           (submod ".." definitions))
  (provide (all-defined-out))
  (define (declarative-environment? v)
    (is-a? v declarative-environment%))
  (define (object-environment? v)
    (is-a? v object-environment%))
  (define (function-environment? v)
    (is-a? v function-environment%))
  (define (global-environment? v)
    (is-a? v global-environment%))
  (define (environment? v)
    (or (declarative-environment? v)
        (object-environment? v)
        (function-environment? v)
        (global-environment? v))))

(require typed/racket/unsafe
         (submod "." definitions))
; FIXME: unsafe
(unsafe-require/typed (submod "." predicates)
                      [declarative-environment? (-> Any Boolean : ESDeclarativeEnvironment)]
                      [object-environment? (-> Any Boolean : ESObjectEnvironment)]
                      [function-environment? (-> Any Boolean : ESFunctionEnvironment)]
                      [global-environment? (-> Any Boolean : ESGlobalEnvironment)]
                      [environment? (-> Any Boolean : ESEnvironment)])
(provide (all-from-out (submod "." definitions))
         environment?
         (all-defined-out))

(require typed/racket/class
         "../convert.rkt"
         "object.rkt"
         "primitive.rkt"
         "string.rkt")

(require/typed "error.rkt" ; TODO
               [raise-native-error (->* (Symbol) ((U String ESString)) Nothing)])

(require/typed "realm.rkt" ; TODO
               [get-global-object (-> ESObject)])

(struct reference
  ([base : (U (Boxof Any) ESEnvironment 'unresolvable)]
   [name :  ESPropertyKey]
   [strict? : Boolean]
   [this-value : (U (Boxof Any) 'empty)])
  #:type-name ESReference
  #:transparent)

(define (property-reference? [v : ESReference])
  (box? (reference-base v)))

(define (unresolvable-reference? [v : ESReference])
  (eq? 'unresolvable (reference-base v)))

(define (super-reference? [v : ESReference])
  (not (eq? 'empty (reference-this-value v))))

(define (private-reference? [v : ESReference])
  ; TODO
  #f)

(: get-this-value (-> ESReference Any))

(define (get-value v)
  (cond
    [(not (reference? v)) v]
    [(unresolvable-reference? v) (raise-native-error 'reference)]
    [(property-reference? v)
     (let ([base (to-object (unbox (reference-base v)))])
       ; TODO: private reference
       (send base get-property (reference-name v) (get-this-value v)))]
    [else
     (let ([base (reference-base v)])
       (assert base environment?)
       (send base get-binding-value (cast (reference-name v) ESString) (reference-strict? v)))]))

(define (put-value! v w)
  (cond
    [(not (reference? v)) (raise-native-error 'reference)]
    [(unresolvable-reference? v)
     (when (reference-strict? v) (raise-native-error 'reference))
     (set-property! (get-global-object) (reference-name v) w #f)]
    [(property-reference? v)
     (let ([base (to-object (unbox (reference-base v)))])
       ; TODO: private reference
       (let ([status (send base set-property! (reference-name v) w (get-this-value v))])
         (when (and (not status) (reference-strict? v)) (raise-native-error 'type))))]
    [else
     (let ([base (reference-base v)])
       (assert base environment?)
       (send base set-mutable-binding! (cast (reference-name v) ESString) w (reference-strict? v)))]))

(define (get-this-value v)
  (assert (property-reference? v))
  (if (super-reference? v)
      (unbox (reference-this-value v))
      (unbox (reference-base v))))

(define (initialize-referenced-binding! v w)
  (assert v reference?)
  (assert (not (unresolvable-reference? v)))
  (let ([base (reference-base v)])
    (assert (environment? base))
    (send base initialize-binding! (cast (reference-name v) ESString) w)))

; TODO: update

(define (new-declarative-environment [e : (U ESNull ESEnvironment)])
  (new declarative-environment%
    [outer-env e]))

(define (new-object-environment [o : ESObject] [e : (U ESNull ESEnvironment)])
  (new object-environment%
    [binding-object o]
    [is-with-environment? #f]
    [outer-env e]))

(define (create-variables! [e : ESEnvironment] [ids : (Listof Symbol)])
  (for ([id (in-list ids)])
    (send e create-mutable-binding! (string->es-string (symbol->string id)) #f)))

; TODO: remove

(define (environment-record? v) (environment? v))

(define (make-property-reference [base : Any] [name : ESString] [strict? : Boolean])
  (reference (box base) name strict? 'empty))
