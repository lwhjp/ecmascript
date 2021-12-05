#lang racket/base

(require racket/lazy-require
         "primitive.rkt"
         "string.rkt")

(lazy-require
 ["../convert.rkt" (to-string)])

(provide (all-defined-out))

(struct es-exn exn:fail (value) #:transparent)

;; TODO: error constructors should be intrinsics
(define native-error-constructor (make-parameter #f))

(define (raise-native-error type [message ecma:undefined])
  (raise-es-exn ((native-error-constructor) type message)))

(define (raise-es-exn v)
  (define message
    (es-string->string
     (with-handlers ([exn:fail? (λ (e)
                                  "(invalid)")])
       (to-string v))))
  (raise (es-exn message (current-continuation-marks) v)))

(define-syntax-rule
  (with-es-exceptions form ...)
  (with-handlers ()
    form ...))
