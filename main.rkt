#lang racket/base

(require "convert.rkt"
         "eval.rkt"
         "function.rkt"
         "init.rkt"
         "types.rkt"
         "private/environment.rkt"
         "private/function.rkt"
         "private/global-object.rkt"
         "private/literal.rkt"
         "private/operator.rkt"
         "private/statement.rkt")

(provide (all-from-out "private/literal.rkt"
                       "private/operator.rkt"
                       "private/statement.rkt")
         ; TODO: several of these should be removed
         global-object
         new-object-environment
         lexical-environment
         get-value
         put-value!
         begin-scope
         id
         member

         undefined
         null

         function
         return
         this
         call
         new

         (rename-out
          [ecma:top-interaction #%top-interaction])

         #%module-begin
         #%app
         #%datum)

(define-syntax-rule (ecma:top-interaction . form)
  (get-value form))
