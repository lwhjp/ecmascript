#lang racket/base

(require racket/class
         "function.rkt"
         "object.rkt"
         "scope.rkt"
         "types.rkt")

(provide (rename-out
          [ecma:member member]
          [ecma:new new]
          [ecma:call call]))

(define (ecma:member obj id)
  (reference
   (to-object (get-value obj))
   (to-string (get-value id))))

(define (ecma:new class . args)
  (define classv (get-value class))
  (define argsv (map get-value args))
  (unless (is-a? classv constructor%)
    (error 'new "not a constructor"))
  (define obj
    (send classv construct . argsv))
  (unless (object? obj)
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
