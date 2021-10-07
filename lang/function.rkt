#lang racket/base

(require (for-syntax racket/base
                     syntax/parse)
         racket/class
         racket/stxparam
         "../private/environment.rkt"
         "../private/error.rkt"
         "../private/function.rkt"
         "../private/object.rkt"
         "../private/primitive.rkt"
         "../private/realm.rkt"
         "../private/this.rkt"
         "../lib/function.rkt"
         "../convert.rkt"
         "environment.rkt")

(provide
 return
 function
 begin-scope
 (rename-out
  [ecma:call call]
  [ecma:new new]
  [ecma:this this]))

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
      [(_ v) #'(return-binding (get-value v))])))

(define-syntax function
  (syntax-parser
   [(_ (~optional name:id)
       (param:id ...)
       (~optional (~seq #:vars (var-id:id ...)))
       (~optional (~seq body:expr ...+)))
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
                                           #,@(if (attribute var-id) #'(#:vars (var-id ...)) #'())
                                           #,@(or (attribute body) #'(ecma:undefined)))))))])])
          #,@(if (attribute name)
                 (with-syntax ([idstr (symbol->string (syntax-e (attribute name)))])
                   #'((send scope-env create-immutable-binding! idstr)
                      (send scope-env initialize-immutable-binding! idstr f)))
                 #'())
          f))]))

(define-syntax (begin-scope stx)
  (syntax-parse stx
    [(_ new-env (~optional (~seq #:vars (var-id:id ...))) form ...)
     (with-syntax ([var-ids (or (attribute var-id) '())])
       #'(let ([new-scope new-env])
           (syntax-parameterize
               ([variable-environment (make-rename-transformer #'new-scope)]
                [lexical-environment (make-rename-transformer #'new-scope)])
             (reorder-functions () ()
               (create-variables! variable-environment 'var-ids)
               form ...))))]))

(define (create-function! env-rec id fn)
  (let ([name (symbol->string id)])
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

(define (ecma:call ref . args)
  (let ([func (get-value ref)])
    (unless (Function? func)
      (raise-native-error 'type "not a function"))
    (let ([this-value
           (if (reference? ref)
               (let ([base (reference-base ref)])
                 (cond
                   [(Object? base) base]
                   [(is-a? base environment-record%)
                    (send base implicit-this-value)]))
               ecma:undefined)])
      (let ([argvs (map get-value args)])
        (send func call
              (cond
                [(or (ecma:null? this-value)
                     (ecma:undefined? this-value))
                 (current-global-object)]
                [(Object? this-value) this-value]
                [else (to-object this-value)])
              argvs)))))

(define (ecma:new ref . args)
  (let ([constructor (get-value ref)])
    (unless (Function? constructor)
      (raise-native-error 'type "not a constructor"))
    (let ([argvs (map get-value args)])
      (send constructor construct argvs))))
