#lang racket/base

(require (for-syntax racket/base)
         racket/class
         racket/match
         racket/stxparam
         "error.rkt"
         "global-object.rkt"
         "object.rkt"
         (prefix-in ecma: "../types.rkt"))

(provide (struct-out reference)
         get-value
         put-value!
         environment-record%
         new-declarative-environment
         new-object-environment
         global-environment
         variable-environment
         lexical-environment
         begin-scope
         id
         member
         declare-vars
         declare-fn)

(struct reference (base name strict?) #:transparent)

(define (get-value v)
  (if (reference? v)
      (let ([base (reference-base v)])
        (cond
          [(eq? 'undefined base)
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
          [(is-a? base ecma-object%)
           (send base get (reference-name v))]
          [else
           (let ([o (ecma:to-object base)]
                 [p (reference-name v)])
             (let ([prop (send o get-property p)])
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
  (unless (reference? v)
    (raise-native-error 'reference "not a reference"))
  (let ([base (reference-base v)])
    (cond
      [(eq? 'undefined base)
       (if (reference-strict? v)
           (raise-native-error 'reference "not bound")
           (send global-object put!
                 (reference-name v)
                 w
                 #f))]
      [(is-a? base environment-record%)
       (send base
             set-mutable-binding!
             (reference-name v)
             w
             (reference-strict? v))]
      [(is-a? base ecma-object%)
       (send base
             put!
             (reference-name v)
             w
             (reference-strict? v))]
      [else
       (let ([o (ecma:to-object base)]
             [p (reference-name v)]
             [throw? (reference-strict? v)])
         (if (and
              (send o can-put? p)
              (not (data-property?
                    (send o get-own-property p))))
             (let ([prop (send o get-property p)])
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
    (define/override (create-mutable-binding! n d)
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
      (send binding-object has-property? n))
    (define/override (create-mutable-binding! n d)
      (send binding-object
            define-own-property
            n
            `(data
              (value . undefined)
              (writable . #t)
              (enumerable . #t)
              (configurable . ,d))
            #t))
    (define/override (set-mutable-binding! n v s)
      (send binding-object put! n v s))
    (define/override (get-binding-value n s)
      (if (send binding-object has-property? n)
          (send binding-object get n)
          (if s
              (raise-native-error
               'reference
               (format "~a: undefined" n))
              'undefined)))
    (define/override (delete-binding! n)
      (send binding-object delete! n #f))
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
      (reference 'undefined name strict?)
      (if (send lex has-binding? name)
          (reference lex name strict?)
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

(define-syntax (begin-scope stx)
  (syntax-case stx ()
    [(_ obj form0 form ...)
     #'(let ([new-scope (new-object-environment obj lexical-environment)])
         (syntax-parameterize
          ([variable-environment (make-rename-transformer #'new-scope)]
           [lexical-environment (make-rename-transformer #'new-scope)])
          form0 form ...))]))

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

(define-syntax-rule (declare-vars (id ...))
  (create-variables!
   variable-environment
   '(id ...)))

(define (member obj id)
  (reference
   (ecma:to-object (get-value obj))
   (ecma:to-string (get-value id))
   #f))

(define (create-function! env-rec id fn)
  (let ([name (symbol->string id)])
    (void
     (send env-rec create-mutable-binding! name #f)
     (send env-rec set-mutable-binding! name fn #f))))

(define-syntax-rule (declare-fn id fn)
  (create-function!
   variable-environment
   'id
   fn))
