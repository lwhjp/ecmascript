#lang racket/base

(require (for-syntax racket/base
                     syntax/parse)
         (only-in racket/class get-field send)
         racket/provide
         racket/stxparam
         "environment.rkt"
         "function.rkt"
         "object.rkt"
         "../convert.rkt"
         "../object.rkt"
         "../types.rkt"
         (prefix-in ecma: "operator.rkt"))

(provide (filtered-out
          (λ (name)
            (and (regexp-match? #rx"^stmt:" name)
                 (substring name 5)))
          (all-defined-out)))

(struct exn:throw exn (value) #:transparent)

(define-syntax-parameter break-bindings '())

(define-syntax-parameter continue-bindings '())

(define-syntax-parameter return-value #f)

(define-syntax (stmt:break stx)
  (let ([breaks (syntax-parameter-value #'break-bindings)])
    (when (null? breaks)
      (raise-syntax-error #f "invalid outside of loop" stx))
    (syntax-case stx ()
      [(_) #`(#,(cdar breaks)
              return-value)]
      [(_ label)
       (let ([loop (assv (syntax-e #'label) breaks)])
         (unless loop
           (raise-syntax-error #f "no such label" stx #'label))
         (with-syntax ([break-binding (cdr loop)])
           #'(break-binding return-value)))])))

(define-syntax (stmt:continue stx)
  (let ([continues (syntax-parameter-value #'continue-bindings)])
    (when (null? continues)
      (raise-syntax-error #f "invalid outside of loop" stx))
    (syntax-case stx ()
      [(_) #`(#,(cdar continues)
              return-value)]
      [(_ label)
       (let ([loop (assv (syntax-e #'label) continues)])
         (unless loop
           (raise-syntax-error #f "no such label" stx #'label))
         (with-syntax ([continue-binding (cdr loop)])
           #'(continue-binding return-value)))])))

(define-syntax stmt:block
  (syntax-rules ()
    [(_) (void)]
    [(_ stmt ...) (begin stmt ...)]))

(define-syntax-rule (var [var-id init] ...)
  (begin
    (put-value! (id var-id) (get-value init)) ...))

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
                         ([break-bindings (cons
                                           (cons '#,(syntax-property stx 'label)
                                                 #'escape)
                                           (syntax-parameter-value #'break-bindings))]
                          [continue-bindings (cons
                                              (cons '#,(syntax-property stx 'label)
                                                    #'next)
                                              (syntax-parameter-value #'continue-bindings))]
                          [return-value (make-rename-transformer #'rv)])
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
            (put-value! lhs (get obj name))
            body)))))

(define-syntax-rule (stmt:with expr body0 body ...)
  (begin-scope (new-object-environment (get-value expr) lexical-environment)
    body0 body ...))

(define-syntax (stmt:switch stx)
  (syntax-case stx ()
    [(_ expr clause ...)
     #'(let/ec escape
         (syntax-parameterize
             ([break-bindings (cons (cons #f #'escape)
                                    (syntax-parameter-value #'break-bindings))]
              [return-value (λ (stx) #'(void))])
           (let ([v expr])
             (do-switch v first-lbl () () ([else (escape)]) clause ...))))]))

(define-syntax do-switch
  (syntax-rules (default)
    [(_ _ end (letrec-part ...) (cond-part ...) (default-part ...))
     (letrec (letrec-part ... [end void])
       (cond cond-part ... default-part ...))]
    [(_ v lbl (letrec-part ...) (cond-part ...) (default-part ...) (default stmt ...) clause ...)
     (do-switch v next-lbl
                (letrec-part ...
                 [lbl (λ () stmt ... (next-lbl))])
                (cond-part ...)
                ([else (lbl)])
                clause ...)]
    [(_ v lbl (letrec-part ...) (cond-part ...) (default-part ...) (test stmt ...) clause ...)
     (do-switch v next-lbl
                (letrec-part ...
                 [lbl (λ () stmt ... (next-lbl))])
                (cond-part ...
                 [(ecma:=== v test) (lbl)])
                (default-part ...)
                clause ...)]))

(define-syntax (stmt:label stx)
  (syntax-case stx ()
    [(_ label stmt)
     (unless (identifier? #'label)
       (raise-syntax-error #f 'syntax "invalid label" stx #'label))
     (syntax-property #'stmt 'label (syntax-e #'label))]))

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
               (with-syntax ([eid (symbol->string (syntax-e (attribute cid)))])
                 #'([exn:throw?
                     (λ (e)
                       (let ([env (new-declarative-environment lexical-environment)])
                         (send env create-mutable-binding! eid)
                         (send env set-mutable-binding! eid (exn:throw-value e) #f)
                         (begin-scope env
                           cbody)))]))
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
