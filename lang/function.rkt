#lang racket/base

(require (for-syntax racket/base
                     syntax/parse)
         racket/class
         racket/stxparam
         "../private/environment.rkt"
         "../private/error.rkt"
         "../private/function.rkt"
         "../private/primitive.rkt"
         "../private/string.rkt"
         "../private/this.rkt"
         "../lib/function.rkt"
         "environment.rkt"
         "reference.rkt")

(provide
 return
 function
 begin-scope
 (rename-out
  [ecma:new new]
  [ecma:this this]
  [es:var var]))

(define-syntax-parameter return-binding #f)

(define-syntax return
  (λ (stx)
    (unless (syntax-parameter-value #'return-binding)
      (raise-syntax-error
       #f
       "not permitted outside of function body"
       stx))
    (syntax-case stx ()
      [(_) #'(return-binding)]
      [(_ v) #'(return-binding v)])))

(define-syntax function
  (syntax-parser
   [(_ (~optional name:id)
       (param:id ...)
       (~optional (~seq #:vars (var-id:id ...)))
       (~optional (~seq body:expr ...+) #:defaults ([(body 1) (list #'ecma:undefined)])))
    #`(let ([scope-env (new-declarative-environment lexical-environment)])
        (letrec
            ([f (new Function%
                     [formal-parameters '(param ...)]
                     [proc
                      (λ args
                        (let ([local-env (new-declarative-environment scope-env)])
                          (send f bind-arguments args local-env)
                          (let/ec escape
                            (syntax-parameterize
                                ([return-binding (make-rename-transformer #'escape)])
                              (begin-scope local-env
                                           (~? (~@ #:vars (var-id ...)))
                                body ...)))))])])
          #,@(if (attribute name)
                 (with-syntax ([idstr #'(string->es-string (symbol->string 'name))])
                   #'((send scope-env create-immutable-binding! idstr)
                      (send scope-env initialize-immutable-binding! idstr f)))
                 #'())
          f))]))

(begin-for-syntax
  (define-syntax-class var-decl
    #:attributes (name init-stx)
    (pattern name:id
      #:attr init-stx #f)
    (pattern [name:id init:expr]
      #:attr init-stx #'(set-reference! (identifier name) init))))

(define-syntax es:var
  (syntax-parser
    [(_ decl:var-decl ...)
     (with-syntax ([(init ...) (filter values (attribute decl.init-stx))])
       #'(begin init ...))]))

(define-syntax (begin-scope stx)
  (syntax-parse stx
    [(_ new-env (~optional (~seq #:vars (var-id:id ...))) form ...)
     #'(let ([new-scope new-env])
         (syntax-parameterize
             ([variable-environment (make-rename-transformer #'new-scope)]
              [lexical-environment (make-rename-transformer #'new-scope)])
           (reorder-functions () ()
             (~? (create-variables! variable-environment '(var-id ...)))
             form ...)))]))

(define (create-function! env-rec id fn)
  (let ([name (string->es-string (symbol->string id))])
    (void
     (send env-rec create-mutable-binding! name #f)
     (send env-rec set-mutable-binding! name fn #f))))

(define-syntax reorder-functions
  (syntax-parser
   #:literals (function)
   [(_ ([fn-id fn-def] ...) (form ...))
    #'(begin
        (create-function! variable-environment 'fn-id fn-def) ...
        form ...)]
   [(_ (fns ...) collected-forms
       (function fn-id:id . rest)
       form ...)
    #'(reorder-functions (fns ... [fn-id (function . rest)])
                         collected-forms
        form ...)]
   [(_ collected-fns (collected-form ...)
       form rest ...)
    #'(reorder-functions collected-fns
                         (collected-form ... form)
        rest ...)]))

(define (ecma:new constructor . args)
  (unless (Function? constructor)
    (raise-native-error 'type "not a constructor"))
  (send constructor construct args))
