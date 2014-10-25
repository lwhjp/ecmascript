#lang racket/base

(require "eval.rkt"
         "function.rkt"
         "init.rkt"
         "types.rkt"
         "private/environment.rkt"
         "private/function.rkt"
         "private/global-object.rkt"
         "private/helpers.rkt"
         "private/operator.rkt"
         "private/statement.rkt")

(provide (all-from-out "private/helpers.rkt"
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
