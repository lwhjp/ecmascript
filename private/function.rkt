#lang racket/base

(require (for-syntax racket/base
                     syntax/parse)
         (except-in racket/class object?)
         racket/list
         racket/stxparam
         (prefix-in
          ecma:
          (combine-in
           "../convert.rkt"))
         "../object.rkt"
         "environment.rkt"
         "error.rkt"
         "global-object.rkt"
         "object.rkt"
         "primitive.rkt"
         "this.rkt")

(provide Function%
         Function?
         constructor%
         constructor?
         has-instance?
         Function:prototype
         make-function
         return
         function
         begin-scope
         make-native-constructor
         make-native-function
         native-method)

(define Function%
  (class ecma-object%
    (init [prototype Function:prototype])
    (init-field [formal-parameters '()]
                [proc (λ args ecma:undefined)])
    (super-new [class-name 'Function]
               [prototype prototype])))

(define (Function? v) (is-a? v Function%))

(define constructor%
  (class Function%
    (init-field new-proc)
    (super-new)))

(define (constructor? v) (is-a? v constructor%))

(define (has-instance? f v)
  (and
   (Object? v)
   (let ([o (get-property-value f "prototype")])
     (unless (Object? o)
       (raise-native-error 'type "not an object"))
     (let loop ([v v])
       (let ([v (get-field prototype v)])
         (and
          v
          (or (eq? o v)
              (loop v))))))))

(define Function:prototype
  (new Function% [prototype Object:prototype]))

(define Arguments%
  (class ecma-object%
    (init [prototype Object:prototype])
    (super-new [class-name 'Arguments]
               [prototype prototype])))

(define (make-arguments-object f args env)
  (define formal-parameters (get-field formal-parameters f))
  (let ([arg-obj (new Arguments%)])
    (for ([arg-name (in-sequences (in-list (map symbol->string formal-parameters))
                                  (in-cycle (list #f)))]
          [arg-v (in-sequences (in-list args)
                               (in-cycle (list ecma:undefined)))]
          [index (in-range (max (length formal-parameters)
                                (length args)))])
      (define-own-property arg-obj
        (number->string index)
        `(data (value . ,arg-v)
               (writable . #t)
               (enumerable . #t)
               (configurable . #t))
        #f)
      (when arg-name
        (define-own-property arg-obj
          arg-name
          `(accessor (get . ,(λ () (send env get-binding-value arg-name #t)))
                     (set . ,(λ (v) (send env set-mutable-binding! arg-name v #t)))
                     (configurable . #t))
          #f)))
    (define-own-property arg-obj
          "length"
          `(data (value . ,(length args))
                 (writable . #t)
                 (enumerable . #f)
                 (configurable . #t))
          #f)
    (define-own-property arg-obj
          "callee"
          `(data (value . ,f)
                 (writable . #t)
                 (enumerable . #f)
                 (configurable . #t))
          #f)
    arg-obj))

(define (bind-arguments! f formal-parameters env args)
  (for ([arg-name (in-list (map symbol->string formal-parameters))]
        [v (in-sequences (in-list args)
                         (in-cycle (list ecma:undefined)))])
    (unless (send env has-binding? arg-name)
      (send env create-mutable-binding! arg-name))
    (send env set-mutable-binding! arg-name v #f))
  (unless (send env has-binding? "arguments")
    (send env create-immutable-binding! "arguments"))
  (send env initialize-immutable-binding!
        "arguments"
        (make-arguments-object f args env)))

(define (make-function params proc)
  (letrec
      ([f (new constructor%
               [formal-parameters params]
               [proc proc]
               [new-proc
                (λ args
                  (let* ([prot (get-property-value f "prototype")]
                         [prot (if (Object? prot) prot Object:prototype)]
                         [obj (new Object% [prototype prot])]
                         [result (apply/this obj proc args)])
                    (if (Object? result)
                        result
                        obj)))])]
       [proto (new Object%)])
    (define-own-property f
          "length"
          `(data
            (value . ,(length params))
            (writable . #f)
            (enumerable . #f)
            (configurable . #f))
          #f)
    (define-own-property proto
          "constructor"
          `(data
            (value . ,f)
            (writable . #t)
            (enumerable . #f)
            (configurable . #t))
          #f)
    (define-own-property f
          "prototype"
          `(data
            (value . ,proto)
            (writable . #t)
            (enumerable . #f)
            (configurable . #f))
          #f)
    f))

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
            ([f (make-function '(param ...)
                  (λ args                     
                    (let ([local-env (new-declarative-environment scope-env)])
                       (bind-arguments! f '(param ...) local-env args)
                       (let/ec escape
                         (syntax-parameterize
                             ([return-binding (make-rename-transformer #'escape)])
                           (begin-scope local-env
                             #,@(if (attribute var-id) #'(#:vars (var-id ...)) #'())
                             #,@(or (attribute body) #'(ecma:undefined))))))))])
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

(define (create-function! env-rec id fn)
  (let ([name (symbol->string id)])
    (void
     (send env-rec create-mutable-binding! name #f)
     (send env-rec set-mutable-binding! name fn #f))))

(define (make-native-constructor call-proc construct-proc)
  (new constructor%
       [formal-parameters 'TODO]
       [proc call-proc]
       [new-proc construct-proc]))

(define (typical-arity proc)
  (let ([arity (procedure-arity proc)])
    (cond
      [(list? arity)
       (argmax typical-arity arity)]
      [(arity-at-least? arity)
       (arity-at-least-value arity)]
      [else arity])))

(define (make-native-function proc)
  (define arity
    (typical-arity proc))
  (define wrapper
    (λ args
      (if (procedure-arity-includes? proc (length args))
          (apply proc args)
      (apply
       proc
       (for/list ([i (in-range arity)]
                  [arg (in-sequences args (in-cycle (list ecma:undefined)))])
         arg)))))
  (define f
    (new Function%
         [formal-parameters 'TODO]
         [proc wrapper]))
  (define-own-property f
        "length"
        `(data
          (value . ,typical-arity)
          (writable . #f)
          (enumerable . #f)
          (configurable . #f))
        #f)
  f)

(define-syntax-rule (native-method args body0 body ...)
  (make-native-function
   (λ args body0 body ...)))
