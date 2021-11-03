#lang racket/base

(require (for-syntax racket/base)
         "../private/environment.rkt"
         "../private/realm.rkt"
         "environment.rkt"
         "function.rkt"
         "literal.rkt"
         "operator.rkt"
         "statement.rkt"
         "../convert.rkt"
         "../function.rkt"
         "../types.rkt")

(provide (all-from-out "literal.rkt"
                       "operator.rkt"
                       "statement.rkt")
         ; TODO: several of these should be removed
         current-global-object
         new-object-environment
         lexical-environment
         get-value
         put-value!
         begin-scope
         expression
         id
         member
         var

         undefined
         null

         function
         return
         this
         call
         new

         (rename-out
          [ecma:top-interaction #%top-interaction]
          [ecma:top #%top])

         #%module-begin
         #%app
         #%datum)

(define-syntax-rule (ecma:top-interaction . form)
  (get-value form))

(define-syntax (ecma:top stx)
  (syntax-case stx ()
    [(_ . v) #'(id v)]))
