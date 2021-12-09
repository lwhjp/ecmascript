#lang racket/base

(require (for-syntax racket/base)
         "../private/error.rkt"
         "../private/realm.rkt"
         "function.rkt"
         "helpers.rkt"
         "literal.rkt"
         "operator.rkt"
         "reference.rkt"
         "statement.rkt"
         "../types.rkt")

(provide (all-from-out "helpers.rkt"
                       "literal.rkt"
                       "operator.rkt"
                       "statement.rkt")
         identifier
         member
         delete
         typeof
         var

         null

         function
         return
         this
         call
         new

         (rename-out
          [es-module-begin #%module-begin]
          [es-top-interaction #%top-interaction]
          [es-top #%top])

         #%app
         #%datum)

(define current-module-environment (make-parameter #f))

(define-syntax (es-module-begin stx)
  (syntax-case stx ()
    [(_ #:vars (var ...) stmt ...)
     #'(#%plain-module-begin
        (current-module-environment (realm-global-env (current-realm)))
        (begin-scope (current-module-environment)
          #:vars (var ...)
          stmt ...))]))

(define-syntax (es-top-interaction stx)
  (syntax-case stx (begin)
    [(_ . (begin #:vars (var ...) form ...))
     #'(begin-scope (current-module-environment)
          #:vars (var ...)
          form ...)]))

(define-syntax (es-top stx)
  (syntax-case stx ()
    [(_ . v) #'(identifier v)]))
