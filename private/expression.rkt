#lang racket/base

(require (for-syntax racket/base)
         (only-in racket/class is-a? send)
         racket/provide
         racket/stxparam
         "environment.rkt"
         "error.rkt"
         "function.rkt"
         "object.rkt"
         "../types.rkt")

(provide (filtered-out
          (λ (name)
            (and (regexp-match? #rx"^ecma:" name)
                 (substring name 5)))
          (all-defined-out)))

(define-syntax ecma:this
  (make-parameter-rename-transformer #'this-binding))

(define (ecma:member obj id)
  (reference
   (to-object (get-value obj))
   (to-string (get-value id))
   #f))

(define (ecma:new class . args)
  (define classv (get-value class))
  (define argsv (map get-value args))
  (unless (constructor? classv)
    (raise-native-error 'type "not a constructor"))
  (define obj
    (send classv construct . argsv))
  (unless (object? obj)
    (raise-native-error 'type "constructor did not produce an object"))
  obj)

(define (ecma:call f . args)
  (define fv (get-value f))
  (define argsv (map get-value args))
  (unless (function? fv)
    (raise-native-error 'type "not a function"))
  (let ([this (if (and (reference? f)
                       (not (eq? 'null (reference-base f)))
                       (not (is-a? (reference-base f) activation%)))
                  (reference-base f)
                  'null)])
    (send fv call this . argsv)))
