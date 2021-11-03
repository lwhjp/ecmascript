#lang racket/base

(require racket/class
         racket/lazy-require
         "error.rkt"
         "object.rkt"
         "primitive.rkt"
         "realm.rkt")

(lazy-require
 ["../convert.rkt" (to-object)])

(provide expression
         get-value
         put-value!
         environment-record%
         new-declarative-environment
         new-object-environment
         get-identifier-reference
         create-variables!)

(define-syntax-rule (expression e)
  (call-with-values
   (λ () e)
   (case-lambda
     [(v) (get-value v)]
     [() (void)])))

(define (get-value v)
  (if (reference? v)
      (let ([base (reference-base v)])
        (cond
          [(ecma:undefined? base)
           (raise-native-error
            'reference
            (format
             "~a: undefined"
             (reference-name v)))]
          [(is-a? base environment-record%)
           (send base
                 get-binding-value
                 (reference-name v)
                 (reference-strict? v))]
          [(Object? base)
           (get-property-value base (reference-name v))]
          [else
           (let ([o (to-object base)]
                 [p (reference-name v)])
             (let ([prop (get-property o p)])
               (cond
                 [(data-property? prop)
                  (data-property-value prop)]
                 [(accessor-property? prop)
                  (let ([getter (accessor-property-get prop)])
                    (if getter
                        (send getter call base)
                        ecma:undefined))]
                 [else ecma:undefined])))]))
      v))

(define (put-value! v w)
  (unless (reference? v)
    (raise-native-error 'reference "not a reference"))
  (let ([base (reference-base v)])
    (cond
      [(ecma:undefined? base)
       (if (reference-strict? v)
           (raise-native-error 'reference "not bound")
           (set-property-value!
            (current-global-object)
            (reference-name v)
            w
            #f))]
      [(is-a? base environment-record%)
       (send base
             set-mutable-binding!
             (reference-name v)
             w
             (reference-strict? v))]
      [(Object? base)
       (set-property-value!
        base
        (reference-name v)
        w
        (reference-strict? v))]
      [else
       (let ([o (to-object base)]
             [p (reference-name v)]
             [throw? (reference-strict? v)])
         (if (and
              (can-set-property? o p)
              (not (data-property?
                    (get-own-property o p))))
             (let ([prop (get-property o p)])
               (if (accessor-property? prop)
                   (send (accessor-property-set prop)
                         call
                         base
                         w)
                   (when throw?
                     (raise-native-error 'type))))
             (when throw?
               (raise-native-error 'type))))])))

(define environment-record%
  (class object%
    (init-field outer)
    (super-new)
    (abstract
     has-binding?
     create-mutable-binding!
     set-mutable-binding!
     get-binding-value
     delete-binding!
     implicit-this-value)))

(struct mutable-binding ([value #:mutable] deletable?) #:prefab)
(struct immutable-binding (value) #:prefab)

(define declarative-environment-record%
  (class environment-record%
    (define bindings (make-hash))
    (super-new)
    (define/override (has-binding? n)
      (hash-has-key? bindings n))
    (define/override (create-mutable-binding! n [d #f])
      (hash-set! bindings n (mutable-binding ecma:undefined d)))
    (define/override (set-mutable-binding! n v s)
      (let ([b (hash-ref bindings n)])
        (if (mutable-binding? b)
            (set-mutable-binding-value! b v)
            (when s
              (raise-native-error
               'type
               (format "~a: not a mutable binding" n))))))
    (define/override (get-binding-value n s)
      (let ([b (hash-ref bindings n)])
        (cond
          [(mutable-binding? b) (mutable-binding-value b)]
          [(immutable-binding? b) (immutable-binding-value b)]
          [else (if s
                    (raise-native-error
                     'reference
                     (format "~a: not bound" n))
                    ecma:undefined)])))
    (define/override (delete-binding! n)
      (let ([b (hash-ref bindings n #f)])
        (cond
          [(not b) #t]
          [(and (mutable-binding? b)
                (mutable-binding-deletable? b))
           (hash-remove! bindings n)
           #t]
          [else #f])))
    (define/override (implicit-this-value)
      ecma:undefined)
    (define/public (create-immutable-binding! n)
      (hash-set! bindings n 'uninitialized-immutable-binding))
    (define/public (initialize-immutable-binding! n v)
      (hash-set! bindings n (immutable-binding v)))))

(define object-environment-record%
  (class environment-record%
    (init-field binding-object
                [provide-this? #f])
    (super-new)
    (inherit-field outer)
    (define/public (clone)
      (new object-environment-record%
        [outer (if (object? outer) (send outer clone) outer)]
        [binding-object (if (object? binding-object) (send binding-object clone) binding-object)]
        [provide-this? provide-this?]))
    (define/override (has-binding? n)
      (has-property? binding-object n))
    (define/override (create-mutable-binding! n d)
      (define-own-property
            binding-object
            n
            `(data
              (value . ,ecma:undefined)
              (writable . #t)
              (enumerable . #t)
              (configurable . ,d))
            #t))
    (define/override (set-mutable-binding! n v s)
      (set-property-value!
       binding-object
       n
       v
       s))
    (define/override (get-binding-value n s)
      (if (has-property? binding-object n)
          (get-property-value binding-object n)
          (if s
              (raise-native-error
               'reference
               (format "~a: undefined" n))
              ecma:undefined)))
    (define/override (delete-binding! n)
      (delete-property! binding-object n #f))
    (define/override (implicit-this-value)
      (if provide-this? binding-object ecma:undefined))))

(define (new-declarative-environment e)
  (new declarative-environment-record%
    [outer e]))

(define (new-object-environment o e)
  (new object-environment-record%
    [binding-object o]
    [outer e]))

(define (get-identifier-reference lex name strict?)
  (if (ecma:null? lex)
      (reference ecma:undefined name strict?)
      (if (send lex has-binding? name)
          (reference lex name strict?)
          (get-identifier-reference
           (get-field outer lex)
           name
           strict?))))

(define (create-variables! env-rec ids)
  (for ([id (in-list (map symbol->string ids))])
    (send env-rec create-mutable-binding! id #f)))
