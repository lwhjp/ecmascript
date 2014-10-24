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

(provide (all-defined-out)
         (rename-out [this-binding this])
         return
         function)

(define (function? v)
  (is-a? v function%))

(define (constructor? v)
  (is-a? v constructor%))

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
