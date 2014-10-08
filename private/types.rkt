#lang racket/base

(require racket/math
         racket/string
         (only-in "date.rkt" date-object?)
         "function.rkt"
         "object.rkt")

(provide (all-defined-out))

(define (default-value o [hint (if (date-object? o)
                                   'string
                                   'number)])
  (let/ec return
    (for ([method (if (eq? 'string hint)
                      '("toString" "valueOf")
                      '("valueOf" "toString"))])
      (define f (get o method))
      (when (function-object? f)
        (define v ((function-object-proc f) o))
        (unless (object? v)
          (return v))))
    (error 'default-value "unable to get default value")))

(define (to-primitive v #:preferred-type [preferred #f])
  (if (object? v)
      (if preferred
          (default-value v preferred)
          (default-value v))
      v))

(define (to-boolean v)
  (cond
    [(eq? v 'undefined) #f]
    [(eq? v 'null) #f]
    [(boolean? v) v]
    [(number? v) (not (or (zero? v) (nan? v)))]
    [(string? v) (not (string=? v ""))]
    [(object? v) #t]))

(define (to-number v)
  (cond
    [(eq? v 'undefined) +nan.0]
    [(eq? v 'null) 0]
    [(boolean? v) (if v 1 0)]
    [(number? v) v]
    [(string? v) (or (string->number (string-trim v)) +nan.0)]
    [(object? v) (to-number (to-primitive v 'number))]))

(define (to-integer v)
  (let ([v (to-number v)])
    (if (nan? v)
        0
        (truncate v))))

(define (to-int32 v)
  (let ([v (to-uint32 v)])
    (if (>= v (expt 2 31))
        (- v (expt 2 32))
        v)))

(define (to-uint32 v)
  (let ([v (to-integer v)])
    (if (or (nan? v) (infinite? v))
        0
        (inexact->exact
         (modulo v (expt 2 32))))))

(define (to-uint16 v)
  (let ([v (to-integer v)])
    (if (or (nan? v) (infinite? v))
        0
        (inexact->exact
         (modulo v (expt 2 16))))))

(define (to-string v)
  (cond
    [(eq? v 'undefined) "undefined"]
    [(eq? v 'null) "null"]
    [(boolean? v) (if v "true" "false")]
    [(number? v)
     (cond
       [(nan? v) "NaN"]
       [(infinite? v) "Infinity"]
       [else (number->string v)])]
    [(string? v) v]
    [(object? v) (to-string (to-primitive v #:preferred-type 'string))]))

(define (to-object v)
  (cond
    [(eq? v 'undefined) (error 'to-object "undefined")]
    [(eq? v 'null) (error 'to-object "null")]
    [(boolean? v) (error 'TODO)]
    [(number? v) (error 'TODO)]
    [(string? v) (error 'TODO)]
    [(object? v) v]))
