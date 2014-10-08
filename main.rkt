#lang racket/base

(require "private/expression.rkt"
         "private/function.rkt"
         "private/global.rkt"
         "private/object.rkt"
         "private/operator.rkt"
         "private/scope.rkt"
         "private/statement.rkt")

(provide (all-from-out "private/expression.rkt"
                       "private/operator.rkt"
                       "private/statement.rkt")
         begin-scope
         declare-vars
         function
         get
         get-value
         global-object
         id
         put!
         return
         this

         (rename-out
          [es:module-begin #%module-begin]
          [es:top-interaction #%top-interaction])

         #%app
         #%datum)

(define-syntax-rule (es:module-begin form ...)
  (#%module-begin
   (begin-scope global-object
     form ...)))

(define-syntax-rule (es:top-interaction . form)
  (begin-scope global-object
    (get-value form)))
