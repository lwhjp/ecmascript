#lang racket/base

(require racket/lazy-require)

(lazy-require
 ["function.rkt" (make-native-function)])

(provide
 make-native-function
 native-method)

(define-syntax-rule (native-method args body0 body ...)
  (make-native-function
   (λ args body0 body ...)))
