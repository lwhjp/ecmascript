#lang typed/racket/base

(require racket/provide
         "string.rkt")

(provide (except-out (all-defined-out)
                     (struct-out opaque-primitive))
         ESString
         es-string?)

; TODO: refactor and remove
(provide (rename-out
          [es-undefined ecma:undefined]
          [es-undefined? ecma:undefined?]
          [es-null ecma:null]
          [es-null? ecma:null?]))

(struct (N) opaque-primitive ([name : (∩ Symbol N)])
  #:property prop:custom-write
  (λ (obj port mode)
    (define name (opaque-primitive-name obj))
    (case mode
      [(#f) (write name port)]
      [else (write-string (format "#<~a>" name) port)])))

(define-type ESUndefined (opaque-primitive 'undefined))

(define es-undefined (opaque-primitive 'undefined))

(define-predicate es-undefined? ESUndefined)

(define-type ESNull (opaque-primitive 'null))

(define es-null (opaque-primitive 'null))

(define-predicate es-null? ESNull)

(define-type ESBoolean Boolean)

(define-predicate es-boolean? ESBoolean)

(define-type ESNumber Flonum)

(define-predicate es-number? ESNumber)

(define-type ESBigInt Integer)

(define-predicate es-big-int? ESBigInt)

(define-type ESSymbol Symbol)

(define-predicate es-symbol? ESSymbol)

(define-type ESPrimitive
  (U ESUndefined
     ESNull
     ESBoolean
     ESNumber
     ESBigInt
     ESString
     ESSymbol))

(define-predicate es-primitive? ESPrimitive)
