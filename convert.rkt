#lang racket/base

(require racket/class
         racket/math
         racket/string
         "private/error.rkt"
         "private/function.rkt"
         "private/object.rkt"
         "private/primitive.rkt"
         (only-in "lib/boolean.rkt" make-Boolean)
         (only-in "lib/number.rkt" make-Number)
         (only-in "lib/string.rkt" make-String))

(provide (all-defined-out))

(define (to-primitive v [hint 'number])
  (if (Object? v)
      (let/ec return
        (for ([method (if (eq? 'string hint)
                          '("toString" "valueOf")
                          '("valueOf" "toString"))])
          (let ([f (get-property-value v method)])
            (when (Function? f)
              (let ([v (send f call v '())])
                (unless (Object? v)
                  (return v))))))
        (raise-native-error 'type))
      v))

(define (to-boolean v)
  (cond
    [(ecma:undefined? v) #f]
    [(ecma:null? v) #f]
    [(boolean? v) v]
    [(number? v) (not (or (zero? v) (nan? v)))]
    [(string? v) (not (string=? v ""))]
    [(Object? v) #t]))

(define (to-number v)
  (cond
    [(ecma:undefined? v) +nan.0]
    [(ecma:null? v) 0]
    [(boolean? v) (if v 1 0)]
    [(number? v) v]
    [(string? v) (or (string->number (string-trim v)) +nan.0)]
    [(Object? v) (to-number (to-primitive v 'number))]))

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
    [(ecma:undefined? v) "undefined"]
    [(ecma:null? v) "null"]
    [(boolean? v) (if v "true" "false")]
    [(number? v)
     (cond
       [(nan? v) "NaN"]
       [(infinite? v) "Infinity"]
       [else (number->string v)])]
    [(string? v) v]
    [(Object? v) (to-string (to-primitive v 'string))]))

(define (to-object v)
  (cond
    [(ecma:undefined? v) (raise-native-error 'type "undefined")]
    [(ecma:null? v) (raise-native-error 'type "null")]
    [(boolean? v) (make-Boolean v)]
    [(number? v) (make-Number v)]
    [(string? v) (make-String v)]
    [(Object? v) v]))
