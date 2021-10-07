#lang racket/base

(require (for-syntax racket/base)
         racket/stxparam
         "../private/environment.rkt"
         "../private/primitive.rkt"
         "../private/realm.rkt"
         "../convert.rkt")

(provide (all-defined-out))

(define-syntax (global-environment stx)
  (syntax-case stx ()
    [_ (identifier? stx) #'(current-global-environment)]))

(define-syntax-parameter variable-environment
  (make-rename-transformer #'global-environment))

(define-syntax-parameter lexical-environment
  (make-rename-transformer #'global-environment))

(define-syntax (id stx)
  (syntax-case stx ()
    [(_ sym)
     (unless (identifier? #'sym)
       (raise-syntax-error #f "not an identifier" stx #'sym))
     (with-syntax ([name (symbol->string
                          (syntax-e #'sym))])
       #'(get-identifier-reference
          lexical-environment
          name
          #f))]))

(define-syntax (member stx)
  (syntax-case stx ()
    [(_ base prop)
     #`(reference
        (to-object (get-value base))
        #,(if (identifier? #'prop)
              (symbol->string (syntax-e #'prop))
              #'(to-string (get-value prop)))
        #f)]))
