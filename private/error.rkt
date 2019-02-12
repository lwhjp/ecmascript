#lang racket/base

(require "primitive.rkt")

(provide (all-defined-out))

(define native-error-handler
  (make-parameter #f))

(define (raise-native-error type [message ecma:undefined])
  ((native-error-handler) type message))
