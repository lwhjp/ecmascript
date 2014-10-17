#lang racket/base

(provide (all-defined-out))

(define native-error-handler
  (make-parameter #f))

(define (raise-native-error type [message 'undefined])
  ((native-error-handler) type message))
