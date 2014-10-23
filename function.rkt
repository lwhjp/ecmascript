#lang racket/base

(require (for-syntax racket/base
                     syntax/parse)
         (only-in racket/class is-a? send)
         racket/stxparam
         "private/environment.rkt"
         "private/error.rkt"
         "private/function.rkt"
         "private/global-object.rkt"
         "types.rkt")

(provide (except-out (all-defined-out) return-binding))

(define (function? v)
  (is-a? v function%))

(define (constructor? v)
  (is-a? v constructor%))

(define-syntax-parameter this
  (make-rename-transformer #'global-object))

(define-syntax-parameter return-binding #f)

(define-syntax return
  (λ (stx)
    (unless (syntax-parameter-value #'return-binding)
      (raise-syntax-error
       #f
       "not permitted outside of function body"
       stx))
    (syntax-case stx ()
      [(_) #'(return-binding)]
      [(_ v) #'(return-binding (get-value v))])))

(define-syntax function
  (syntax-parser
   [(_ (~! param:id ...)
       (~optional (~seq #:vars (var-id:id ...)))
       body:expr ...+)
    #`(make-function '(param ...)
        (λ (this-arg activation)
          (begin-scope activation
            #,@(if (attribute var-id) #'(#:vars (var-id ...)) #'())
            (let/ec escape
              (syntax-parameterize
                  ([this (make-rename-transformer #'this-arg)]
                   [return-binding (make-rename-transformer #'escape)])
                body ...)))))]))

(define (call ref . args)
  (let ([func (get-value ref)])
    (unless (function? func)
      (raise-native-error 'type "not a function"))
    (let ([this-value
           (if (reference? ref)
               (let ([base (reference-base ref)])
                 (cond
                   [(object? base) base]
                   [(is-a? base environment-record%)
                    (send base implicit-this-value)]))
               'undefined)])
      (send func call this-value . args))))

(define (new ref . args)
  (let ([constructor (get-value ref)])
    (unless (constructor? constructor)
      (raise-native-error 'type "not a constructor"))
    (send constructor construct . args)))
