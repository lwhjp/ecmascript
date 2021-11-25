#lang racket/base

(require racket/provide)

(provide (matching-identifiers-out #rx"^es-" (all-defined-out)))

; TODO: refactor and remove
(provide (rename-out
          [es-undefined ecma:undefined]
          [es-undefined? ecma:undefined?]
          [es-null ecma:null]
          [es-null? ecma:null?]))

(struct opaque-primitive (name)
  #:methods gen:custom-write
  [(define (write-proc obj port mode)
     (define name (opaque-primitive-name obj))
     (case mode
       [(#f) (write name port)]
       [else (write-string (format "#<~a>" name) port)]))])

(define es-undefined (opaque-primitive 'undefined))

(define (es-undefined? v) (eq? es-undefined v))

(define es-null (opaque-primitive 'null))

(define (es-null? v) (eq? es-null v))

(define es-boolean? boolean?)

(define es-number? flonum?)

(define es-big-int? exact-integer?)

(define es-symbol? symbol?)

(define es-string? string?) ; TODO: UTF-16

(define (es-primitive? v)
  (or (es-undefined? v)
      (es-null? v)
      (es-boolean? v)
      (es-number? v)
      (es-big-int? v)
      (es-symbol? v)
      (es-string? v)))
