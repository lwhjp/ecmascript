#lang racket/base

(require "eval.rkt"
         "init.rkt"
         "private/environment.rkt"
         "private/expression.rkt"
         "private/function.rkt"
         "private/helpers.rkt"
         "private/operator.rkt"
         "private/statement.rkt")

(provide (all-from-out "private/environment.rkt"
                       "private/expression.rkt"
                       "private/helpers.rkt"
                       "private/operator.rkt"
                       "private/statement.rkt")
         function
         return

         (rename-out
          [ecma:module-begin #%module-begin]
          [ecma:top-interaction #%top-interaction])

         #%app
         #%datum)

(define-syntax-rule (ecma:module-begin form ...)
  (#%module-begin
   (current-read-interaction eval-read-interaction)
   form ...))

(define-syntax-rule (ecma:top-interaction . form)
  (get-value form))
