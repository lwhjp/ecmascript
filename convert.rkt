#lang typed/racket/base

(require racket/math
         racket/string
         "lang/helpers.rkt"
         "private/object.rkt"
         "private/primitive.rkt"
         "private/string.rkt")

; TODO
(require/typed "private/error.rkt"
               [raise-native-error (->* (Symbol) (String) Nothing)])
(require/typed "private/function.rkt"
               [#:opaque ESFunction es-function?]
               [call/function (-> ESFunction Any Any * Any)])
(require/typed "lib/boolean.rkt"
               [make-Boolean (-> ESBoolean ESObject)])
(require/typed "lib/number.rkt"
               [make-Number (-> ESNumber ESObject)])
(require/typed "lib/string.rkt"
               [make-String (-> ESString ESObject)])

(provide (except-out (all-defined-out)
                     invalid-value))

(define (invalid-value [who : Symbol] [what : Any])
  (raise-argument-error who "(or/c es-primitive? es-object?)" what))

(define (to-primitive [v : Any] [hint : (U 'string 'number) 'number])
  : ESPrimitive
  (cond
    [(es-object? v)
     (let loop ([methods (if (eq? 'string hint)
                             '("toString" "valueOf")
                             '("valueOf" "toString"))])
       (if (null? methods)
           (raise-native-error 'type)
           (let ([m (get-property-value v (car methods))])
             (if (es-function? m)
                 (let ([result (call/function m v)])
                   (if (es-primitive? result)
                       result
                       (loop (cdr methods))))
                 (loop (cdr methods))))))]
    [(es-primitive? v) v]
    [else (invalid-value 'to-primitive v)]))

(define (to-boolean [v : Any])
  : ESBoolean
  (cond
    [(es-undefined? v) #f]
    [(es-null? v) #f]
    [(es-boolean? v) v]
    [(es-number? v) (not (or (zero? v) (nan? v)))]
    [(es-string? v) (not (es-string=? es-empty-string v))]
    [(es-big-int? v) (not (zero? v))]
    [(es-object? v) #t]
    [else (invalid-value 'to-boolean v)]))

(define (to-numeric [v : Any])
  : (U ESBigInt ESNumber)
  (let ([prim (to-primitive v)])
    (if (es-big-int? v)
        v
        (to-number v))))

(define (to-number [v : Any])
  : ESNumber
  (cond
    [(es-undefined? v) +nan.0]
    [(es-null? v) 0.0]
    [(es-boolean? v) (if v 1.0 0.0)]
    [(es-number? v) v]
    [(es-string? v) (let ([n (string->number (string-trim (es-string->string v)))]) ; FIXME
                      (if (real? n)
                          (cast (exact->inexact n) Flonum)
                          +nan.0))]
    [(es-symbol? v) (raise-native-error 'type)]
    [(es-big-int? v) (raise-native-error 'type)]
    [(es-object? v) (to-number (to-primitive v 'number))]
    [else (invalid-value 'to-number v)]))

(define (to-integer-or-infinity [v : Any])
  (let ([n (to-number v)])
    (if (or (nan? n) (infinite? n))
        0
        (exact-truncate n))))

(define (to-integer [v : Any])
  (let ([n (to-integer-or-infinity v)])
    (if (exact-integer? n)
        n
        0)))

(define (to-int32 [v : Any])
  (let* ([n (to-integer v)]
         [n (modulo n (expt 2 32))])
    (if (>= n (expt 2 31))
        (- n (expt 2 32))
        n)))

(define (to-uint32 [v : Any])
  (modulo (to-integer v) (expt 2 32)))

(define (to-int16 [v : Any])
  (let* ([n (to-integer v)]
         [n (modulo n (expt 2 16))])
    (if (>= n (expt 2 15))
        (- n (expt 2 16))
        n)))

(define (to-uint16 [v : Any])
  (modulo (to-integer v) (expt 2 16)))

(define (to-int8 [v : Any])
  (let* ([n (to-integer v)]
         [n (modulo n (expt 2 8))])
    (if (>= n (expt 2 7))
        (- n (expt 2 8))
        n)))

(define (to-uint8 [v : Any])
  (modulo (to-integer v) (expt 2 8)))

(define (to-big-int [v : Any])
  : ESBigInt
  (let ([prim (to-primitive v)])
    (cond
      [(es-undefined? prim) (raise-native-error 'type)]
      [(es-null? prim) (raise-native-error 'type)]
      [(es-boolean? prim) (if prim 1 0)]
      [(es-big-int? prim) prim]
      [(es-number? prim) (raise-native-error 'type)]
      [(es-string? prim) (error 'TODO)]
      [(es-symbol? prim) (raise-native-error 'type)])))

(define (to-string [v : Any])
  : ESString
  (cond
    [(es-undefined? v) (es-string-literal "undefined")]
    [(es-null? v) (es-string-literal "null")]
    [(es-boolean? v) (if v (es-string-literal "true") (es-string-literal "false"))]
    [(es-number? v)
     (cond
       [(nan? v) (es-string-literal "NaN")]
       [(infinite? v) (es-string-literal "Infinity")]
       [(integer? v) (string->es-string (number->string (inexact->exact v)))]
       [else (string->es-string (number->string v))])]
    [(es-string? v) v]
    [(es-symbol? v) (raise-native-error 'type)]
    [(es-big-int? v) (string->es-string (number->string v))]
    [(es-object? v) (to-string (to-primitive v 'string))]
    [else (invalid-value 'to-string v)]))

(define (to-object [v : Any])
  : ESObject
  (cond
    [(es-undefined? v) (raise-native-error 'type "undefined")]
    [(es-null? v) (raise-native-error 'type "null")]
    [(es-boolean? v) (make-Boolean v)]
    [(es-number? v) (make-Number v)]
    [(es-string? v) (make-String v)]
    [(es-symbol? v) (error 'TODO)]
    [(es-big-int? v) (error 'TODO)]
    [(es-object? v) v]
    [else (invalid-value 'to-object v)]))

(define (to-property-key [v : Any])
  : ESPropertyKey
  (let ([key (to-primitive v 'string)])
    (if (es-symbol? key)
        key
        (to-string key))))
