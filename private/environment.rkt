#lang racket/base

(require racket/class
         "error.rkt"
         "object.rkt"
         "primitive.rkt")

(provide environment-record%
         new-declarative-environment
         new-object-environment
         create-variables!)

(define environment-record%
  (class object%
    (init-field outer)
    (super-new)
    (abstract
     has-binding?
     create-mutable-binding!
     initialize-binding!
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
    (define/override (initialize-binding! n v)
      (hash-set! bindings n (mutable-binding v #f)))
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
    (define/override (initialize-binding! n v)
      (set-mutable-binding! n v #f))
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

(define (create-variables! env-rec ids)
  (for ([id (in-list (map symbol->string ids))])
    (send env-rec create-mutable-binding! id #f)))
