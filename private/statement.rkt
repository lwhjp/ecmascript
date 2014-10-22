#lang racket/base

(require (for-syntax racket/base
                     syntax/parse)
         racket/provide
         racket/stxparam
         "environment.rkt"
         "object.rkt"
         "../types.rkt")

(provide (filtered-out
          (λ (name)
            (and (regexp-match? #rx"^stmt:" name)
                 (substring name 5)))
          (all-defined-out)))

(struct exn:throw exn (value) #:transparent)

(define-syntax-parameter stmt:break
  (λ (stx)
    (raise-syntax-error #f "invalid outside of loop" stx)))

(define-syntax-parameter stmt:continue
  (λ (stx)
    (raise-syntax-error #f "invalid outside of loop" stx)))

(define-syntax stmt:block
  (syntax-rules ()
    [(_) (void)]
    [(_ stmt ...) (begin stmt ...)]))

(define (stmt:empty-statement)
  (void))

(define-syntax (stmt:if stx)
  (syntax-case stx ()
    [(_ test true)
     #'(if test true (void))]
    [(_ test true false)
     #'(if test true false)]))

(define-syntax-rule (stmt:while test body ...)
  (stmt:for #:test test
    body ...))

(define-syntax (stmt:for stx)
  (syntax-parse stx
    [(_ (~optional (~seq #:init init))
        (~optional (~seq #:test test))
        (~optional (~seq #:update update))
        body ...)
     #`(let/ec escape
         #,(or (attribute init) '(void))
         (let loop ([rv (void)])
           (loop
            (let ([new-rv
                   (let/ec next
                     (syntax-parameterize
                         ([stmt:break (λ (stx) #'(escape rv))]
                          [stmt:continue (λ (stx) #'(next rv))])
                       (if #,(if (attribute test)
                                 #'(to-boolean
                                    (get-value test))
                                 #t)
                           (stmt:block
                            body ...)
                           (stmt:break))))])
              #,(or (attribute update) '(void))
              new-rv))))]))

(define-syntax-rule (stmt:for-in lhs expr body)
  (let ([exper-value (get-value expr)])
    (if (or (eq? 'null exper-value)
            (eq? 'undefined exper-value))
        (void)
        (let ([obj (to-object exper-value)])
          (for/fold ([v (void)])
                    ([(name prop) (in-hash
                                   (get-field properties obj))]
                     #:when (property-enumerable? prop))
            (put-value! lhs (send obj get name))
            body)))))

(define-syntax-rule (stmt:with expr body0 body ...)
  (begin-scope expr
    body0 body ...))

(define-syntax-rule (stmt:with-label label stmt)
  ; TODO
  stmt)

(define (stmt:throw expr)
  (let ([v (get-value expr)])
    (raise
     (exn:throw (to-string v)
                (current-continuation-marks)
                v))))

(define-syntax (stmt:try stx)
  (syntax-parse stx
    [(_ body
        (~optional (~seq #:catch cid cbody))
        (~optional (~seq #:finally fbody)))
     (with-syntax
         ([handlers
           (if (attribute cid)
               #'([exn:throw?
                   (λ (e)
                     (declare-vars (cid))
                     (send variable-environment
                           set-mutable-binding!
                           (symbol->string 'cid)
                           (exn:throw-value e)
                           #f)
                     cbody)])
               #'())]
          [post
           (if (attribute fbody)
               #'(λ ()
                   fbody)
               #'void)])
       #'(dynamic-wind
          void
          (λ ()
            (with-handlers handlers
              body))
          post))]))
