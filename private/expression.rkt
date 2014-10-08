#lang racket/base

(require "function.rkt"
         "object.rkt"
         "scope.rkt"
         "types.rkt")

(provide (all-defined-out))

(define (member obj id)
  (reference
   (to-object (get-value obj))
   (to-string (get-value id))))

(define (new class . args)
  (define classv (get-value class))
  (define argsv (map get-value args))
  (unless (constructor-object? classv)
    (error 'new "not a constructor"))
  (define obj
    (apply (constructor-object-proc classv) argsv))
  (unless (object? obj)
    (error 'new "constructor did not produce an object"))
  obj)

(define (call f . args)
  (define fv (get-value f))
  (define argsv (map get-value args))
  (unless (function-object? fv)
    (error 'call "not a function"))
  (let ([this (if (and (reference? f)
                       (not (eq? 'null (reference-base f)))
                       (not (activation-object? (reference-base f))))
                  (reference-base f)
                  'null)])
    (apply (function-object-proc fv) this argsv)))
