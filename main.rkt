#lang racket/base

(require (for-syntax racket/base)
         "private/environment.rkt"
         "private/init.rkt"
         "private/realm.rkt"
         "lang/environment.rkt"
         "lang/function.rkt"
         "lang/literal.rkt"
         "lang/operator.rkt"
         "lang/statement.rkt"
         "convert.rkt"
         "eval.rkt"
         "function.rkt"
         "types.rkt")

(provide (all-from-out "lang/literal.rkt"
                       "lang/operator.rkt"
                       "lang/statement.rkt")
         ; TODO: several of these should be removed
         current-global-object
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

(current-realm (make-realm))
