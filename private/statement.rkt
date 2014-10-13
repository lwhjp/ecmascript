#lang racket/base

(require (for-syntax racket/base
                     syntax/parse)
         racket/provide
         racket/stxparam
         "environment.rkt")

(provide (filtered-out
          (λ (name)
            (and (regexp-match? #rx"^stmt:" name)
                 (substring name 5)))
          (all-defined-out)))

(define-syntax-parameter stmt:break
  (λ (stx)
    (raise-syntax-error #f "invalid outside of loop" stx)))

(define-syntax-parameter stmt:continue
  (λ (stx)
    (raise-syntax-error #f "invalid outside of loop" stx)))

(define-syntax-rule (stmt:block stmt0 stmt ...)
  (begin
    stmt0 stmt ...))

(define (stmt:empty-statement)
  (void))

(define-syntax (stmt:if stx)
  (syntax-case stx ()
    [(_ test true)
     #'(if test true (void))]
    [(_ test true false)
     #'(if test true false)]))

(define-syntax-rule (stmt:while test body0 body ...)
  (stmt:for #:test test
    body0 body ...))

(define-syntax (stmt:for stx)
  (syntax-parse stx
    [(_ (~optional (~seq #:init init))
        (~optional (~seq #:test test))
        (~optional (~seq #:update update))
        body0 body ...)
     #`(let/ec escape
         #,(or (attribute init) '(void))
         (let loop ([rv (void)])
           (loop
            (let ([new-rv
                   (let/ec next
                     (syntax-parameterize
                         ([break (λ (stx) #'(escape rv))]
                          [continue (λ (stx) #'(next rv))])
                       (if #,(or (attribute test) #t)
                           (begin
                             body0 body ...)
                           (break))))])
              #,(or (attribute update) '(void))
              new-rv))))]))

(define-syntax-rule (stmt:with expr body0 body ...)
  (begin-scope expr
    body0 body ...))
