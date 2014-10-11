#lang racket/base

(require "eval.rkt"
         "private/lang.rkt")

(provide (all-from-out "private/lang.rkt")

         (rename-out
          [es:module-begin #%module-begin]
          [es:top-interaction #%top-interaction]))

(define-syntax-rule (es:module-begin form ...)
  (#%module-begin
   (begin-scope (current-global-scope)
     form ...)))

(define-syntax-rule (es:top-interaction . form)
  (begin-scope (current-global-scope)
    (get-value form)))
