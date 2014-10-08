#lang racket/base

(require racket/stxparam
         "scope.rkt"
         (for-syntax racket/base
                     syntax/parse))

(provide break
         continue
         block
         empty-statement
         while
         with
         (rename-out
          [eif if]
          [efor for]))

(define-syntax-parameter break
  (λ (stx)
    (raise-syntax-error #f "invalid outside of loop" stx)))

(define-syntax-parameter continue
  (λ (stx)
    (raise-syntax-error #f "invalid outside of loop" stx)))

(define-syntax-rule (block stmt0 stmt ...)
  (begin
    stmt0 stmt ...))

(define (empty-statement)
  (void))

(define-syntax (eif stx)
  (syntax-case stx ()
    [(_ test true)
     #'(if test true (void))]
    [(_ test true false)
     #'(if test true false)]))

(define-syntax-rule (while test body0 body ...)
  (efor #:test test
    body0 body ...))

(define-syntax (efor stx)
  (syntax-parse stx
    [(_ (~optional (~seq #:init init))
        (~optional (~seq #:test test))
        (~optional (~seq #:step step))
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
              #,(or (attribute step) '(void))
              new-rv))))]))

(define-syntax-rule (with expr body0 body ...)
  (begin-scope expr
    body0 body ...))
