#lang racket/base

(require (for-syntax racket/base)
         racket/class
         racket/provide
         racket/stxparam
         "environment.rkt"
         "function.rkt"
         "object.rkt"
         "types.rkt")

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

(define ecma:null 'null)

(define ecma:undefined 'undefined)

(define (ecma:new class . args)
  (define classv (get-value class))
  (define argsv (map get-value args))
  (unless (is-a? classv constructor%)
    (error 'new "not a constructor"))
  (define obj
    (send classv construct . argsv))
  (unless (is-a? obj ecma-object%)
    (error 'new "constructor did not produce an object"))
  obj)

(define (ecma:call f . args)
  (define fv (get-value f))
  (define argsv (map get-value args))
  (unless (is-a? fv function%)
    (error 'call "not a function"))
  (let ([this (if (and (reference? f)
                       (not (eq? 'null (reference-base f)))
                       (not (is-a? (reference-base f) activation%)))
                  (reference-base f)
                  'null)])
    (send fv call this . argsv)))
