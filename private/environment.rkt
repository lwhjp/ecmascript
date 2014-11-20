#lang racket/base

(require (for-syntax racket/base)
         racket/class
         racket/stxparam
         "../object.rkt"
         "error.rkt"
         "global-object.rkt"
         "object.rkt"
         (prefix-in
          ecma:
          (combine-in
           "../convert.rkt"
           "../types.rkt")))

(provide get-value
         put-value!
         environment-record%
         new-declarative-environment
         new-object-environment
         global-environment
         variable-environment
         lexical-environment
         id
         create-variables!
         member)

(define (get-value v)
  (if (ecma:reference? v)
      (let ([base (ecma:reference-base v)])
        (cond
          [(eq? 'undefined base)
           (raise-native-error
            'reference
            (format
             "~a: undefined"
             (ecma:reference-name v)))]
          [(is-a? base environment-record%)
           (send base
                 get-binding-value
                 (ecma:reference-name v)
                 (ecma:reference-strict? v))]
          [(Object? base)
           (get-property-value base (ecma:reference-name v))]
          [else
           (let ([o (ecma:to-object base)]
                 [p (ecma:reference-name v)])
             (let ([prop (get-property o p)])
               (cond
                 [(data-property? prop)
                  (data-property-value prop)]
                 [(accessor-property? prop)
                  (let ([getter (accessor-property-get prop)])
                    (if getter
                        (send getter call base)
                        'undefined))]
                 [else 'undefined])))]))
      v))

(define (put-value! v w)
  (unless (ecma:reference? v)
    (raise-native-error 'reference "not a reference"))
  (let ([base (ecma:reference-base v)])
    (cond
      [(eq? 'undefined base)
       (if (ecma:reference-strict? v)
           (raise-native-error 'reference "not bound")
           (set-property-value!
            global-object
            (ecma:reference-name v)
            w
            #f))]
      [(is-a? base environment-record%)
       (send base
             set-mutable-binding!
             (ecma:reference-name v)
             w
             (ecma:reference-strict? v))]
      [(Object? base)
       (set-property-value!
        base
        (ecma:reference-name v)
        w
        (ecma:reference-strict? v))]
      [else
       (let ([o (ecma:to-object base)]
             [p (ecma:reference-name v)]
             [throw? (ecma:reference-strict? v)])
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
      (hash-set! bindings n (mutable-binding 'undefined d)))
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
                    'undefined)])))
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
      'undefined)
    (define/public (create-immutable-binding! n)
      (hash-set! bindings n 'uninitialized-immutable-binding))
    (define/public (initialize-immutable-binding! n v)
      (hash-set! bindings n (immutable-binding v)))))

(define object-environment-record%
  (class environment-record%
    (init-field binding-object
                [provide-this? #f])
    (super-new)
    (define/override (has-binding? n)
      (has-property? binding-object n))
    (define/override (create-mutable-binding! n d)
      (define-own-property
            binding-object
            n
            `(data
              (value . undefined)
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
              'undefined)))
    (define/override (delete-binding! n)
      (delete-property! binding-object n #f))
    (define/override (implicit-this-value)
      (if provide-this? binding-object 'undefined))))

(define (new-declarative-environment e)
  (new declarative-environment-record%
    [outer e]))

(define (new-object-environment o e)
  (new object-environment-record%
    [binding-object o]
    [outer e]))

(define (get-identifier-reference lex name strict?)
  (if (eq? 'null lex)
      (ecma:reference 'undefined name strict?)
      (if (send lex has-binding? name)
          (ecma:reference lex name strict?)
          (get-identifier-reference
           (get-field outer lex)
           name
           strict?))))

(define global-environment
  (new-object-environment global-object 'null))

(define-syntax-parameter variable-environment
  (make-rename-transformer #'global-environment))

(define-syntax-parameter lexical-environment
  (make-rename-transformer #'global-environment))

(define-syntax (id stx)
  (syntax-case stx ()
    [(_ sym)
     (unless (identifier? #'sym)
       (raise-syntax-error #f "not an identifier" stx #'sym))
     (with-syntax ([name (symbol->string
                          (syntax-e #'sym))])
       #'(get-identifier-reference
          lexical-environment
          name
          #f))]))

(define (create-variables! env-rec ids)
  (for ([id (in-list (map symbol->string ids))])
    (send env-rec create-mutable-binding! id #f)))

(define-syntax (member stx)
  (syntax-case stx ()
    [(_ base prop)
     #`(ecma:reference
        (ecma:to-object (get-value base))
        #,(if (identifier? #'prop)
              (symbol->string (syntax-e #'prop))
              #'(ecma:to-string (get-value prop)))
        #f)]))
