#lang racket/base

(require (for-syntax racket/base)
         racket/stxparam
         "../private/realm.rkt")

(provide (all-defined-out))

(define-syntax (global-environment stx)
  (syntax-case stx ()
    [_ (identifier? stx) #'(realm-global-env (current-realm))]))

(define-syntax-parameter variable-environment
  (make-rename-transformer #'global-environment))

(define-syntax-parameter lexical-environment
  (make-rename-transformer #'global-environment))
