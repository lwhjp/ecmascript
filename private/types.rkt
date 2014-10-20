#lang racket/base

(require racket/class
         racket/math
         racket/string
         "error.rkt"
         "global-object.rkt"
         "object.rkt")

(provide (all-defined-out))

(define (to-primitive v [preferred #f])
  (if (is-a? v ecma-object%)
      (if preferred
          (send v default-value preferred)
          (send v default-value))
      v))

(define (to-boolean v)
  (cond
    [(eq? v 'undefined) #f]
    [(eq? v 'null) #f]
    [(boolean? v) v]
    [(number? v) (not (or (zero? v) (nan? v)))]
    [(string? v) (not (string=? v ""))]
    [(is-a? v ecma-object%) #t]))

(define (to-number v)
  (cond
    [(eq? v 'undefined) +nan.0]
    [(eq? v 'null) 0]
    [(boolean? v) (if v 1 0)]
    [(number? v) v]
    [(string? v) (or (string->number (string-trim v)) +nan.0)]
    [(is-a? v ecma-object%) (to-number (to-primitive v 'number))]))

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
    [(is-a? v ecma-object%) (to-string (to-primitive v 'string))]))

(define (to-object v)
  (cond
    [(eq? v 'undefined) (raise-native-error 'type "undefined")]
    [(eq? v 'null) (raise-native-error 'type "null")]
    [(boolean? v)
     (send (send global-object get "Boolean")
           construct
           v)]
    [(number? v) (error 'TODO)]
    [(string? v)
     (send (send global-object get "String")
           construct
           v)]
    [(is-a? v ecma-object%) v]))
