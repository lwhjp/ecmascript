#lang racket/base

(require racket/class
         racket/list
         racket/match
         racket/stxparam
         "object.rkt"
         (for-syntax racket/base))

(provide (struct-out reference)
         get-value
         put-value
         begin-scope
         id
         create-variables!
         declare-vars
         declare-fn)

(struct reference (base property) #:transparent)

(define (get-value v)
  (match v
    [(reference 'null _) (error 'undefined)]
    [(reference base prop) (send base get prop)]
    [_ v]))

(define (do-put-value v w scope)
  (match v
    [(reference 'null prop) (send (last scope) put! prop w)]
    [(reference base prop) (send base put! prop w)]
    [_ (error "not a reference")]))

(define-syntax-rule (put-value v w)
  (do-put-value v w scope-chain))

(define-syntax-parameter scope-chain
  (λ (stx)
    #''()))

(define (resolve name scope)
  (cond
    [(null? scope)
     (reference 'null name)]
    [(send (car scope) has-property? name)
     (reference (car scope) name)]
    [else (resolve name (cdr scope))]))

(define-syntax (begin-scope stx)
  (syntax-case stx ()
    [(_ obj form0 form ...)
     #'(let ([new-scope (cons obj scope-chain)])
         (syntax-parameterize
          ([scope-chain (make-rename-transformer #'new-scope)])
          form0 form ...))]))

(define-syntax (id stx)
  (syntax-case stx ()
    [(_ name)
     (let ([p (syntax-e #'name)])
       (unless (symbol? p)
         (raise-syntax-error #f "expected identifier" stx #'name))
       #`(resolve #,(symbol->string p) scope-chain))]))

(define (create-variables! obj defs)
  (for/list ([def defs])
    (match-define (cons name val) def)
    (let ([p (symbol->string name)])
      (unless (send obj has-property? p)
        (send obj put! p val)))))

(define-syntax-rule (declare-vars (id ...))
  (create-variables!
   (first scope-chain)
   '((id . undefined) ...)))

(define-syntax-rule (declare-fn id fn)
  (send (first scope-chain) put! id fn))
