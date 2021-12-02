#lang racket/base

(require racket/lazy-require
         "primitive.rkt"
         "string.rkt")

(lazy-require
 ["../convert.rkt" (to-string)])

(provide (all-defined-out))

(struct es-exn (value) #:transparent)

(struct ecmascript-exception exn:fail:user (value) #:transparent)

;; TODO: error constructors should be intrinsics
(define native-error-constructor (make-parameter #f))

(define (raise-native-error type [message ecma:undefined])
  (raise (es-exn ((native-error-constructor) type message))))

(define-syntax-rule
  (with-es-exceptions form ...)
  (with-handlers ([es-exn? (λ (e)
                             (raise (ecmascript-exception
                                     (es-string->string (to-string (es-exn-value e)))
                                     (current-continuation-marks)
                                     e)))])
    form ...))
