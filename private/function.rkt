#lang racket/base

(require (for-syntax racket/base
                     syntax/parse)
         (except-in racket/class object?)
         racket/list
         racket/stxparam
         (prefix-in
          ecma:
          (combine-in
           "../convert.rkt"
           "../types.rkt"))
         "../object.rkt"
         "environment.rkt"
         "error.rkt"
         "global-object.rkt"
         "object.rkt")

(provide function%
         constructor%
         activation%
         function:prototype
         make-function
         this-binding
         return
         function
         begin-scope
         make-native-constructor
         make-native-function
         native-method)

(define function%
  (class ecma-object%
    (init-field call-proc
                #;scope
                formal-parameters
                #;code)

    (super-new [class "Function"])

    (define/public (call this-arg . args)
      (call-proc
       (cond
         [(or (ecma:null? this-arg) (ecma:undefined? this-arg))
          global-object]
         [(object? this-arg) this-arg]
         [else (ecma:to-object this-arg)])
       (λ (env)
         (bind-arguments! env args))))

    (define/public (has-instance? v)
      (and
       (object? v)
       (let ([o (get-property-value this "prototype")])
         (unless (object? o)
           (raise-native-error 'type "not an object"))
         (let loop ([v v])
           (let ([v (get-field prototype v)])
             (and
              v
              (or (eq? o v)
                  (loop v))))))))

    (define (bind-arguments! env args)
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
              (make-arguments-object this args)))))

(define constructor%
  (class function%
    (init-field)

    (super-new)

    (define/public (construct . args)
      (let* ([prot (get-property-value this "prototype")]
             [prot (if (object? prot) prot object:prototype)]
             [obj (new ecma-object%
                       [class (get-field class prot)]
                       [prototype prot])]
             [result (send this call obj . args)])
        (if (object? result)
            result
            obj)))))

(define native-function%
  (class function%
    (inherit-field call-proc)
    (super-new)
    (define/override (call this-arg . args)
      (apply call-proc this-arg args))))

(define native-constructor%
  (class constructor%
    (inherit-field call-proc)
    (init-field construct-proc)
    (super-new)
    (define/override (call this-arg . args)
      (apply call-proc this-arg args))
    (define/override (construct . args)
      (apply construct-proc args))))

(define activation%
  (class ecma-object%
    (super-new [prototype #f]
               [class "Object"])))

(define function:prototype
  (new function%
       [prototype object:prototype]
       [call-proc (λ args
                    'undefined)]
       [formal-parameters '()]))

(define arguments-object%
  (class ecma-object%
    (init-field arg-map)
    (super-new [class "Arguments"]
               [prototype object:prototype])))

(define (make-arguments-object f args)
  (let* ([arg-map
          (make-hash
           (reverse
            (for/list ([arg (in-list args)]
                       [name (in-list (get-field formal-parameters f))])
              (cons (symbol->string name) arg))))]
         [arg-obj (instantiate arguments-object% (arg-map))])
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

(define (make-function params proc)
  (let ([f (new constructor%
                [prototype function:prototype]
                [call-proc proc]
                [formal-parameters params])]
        [proto
           (new ecma-object%
                [prototype object:prototype]
                [class "Object"])])
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

(define-syntax-parameter this-binding
  (make-rename-transformer #'global-object))

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
        (let ([f (make-function '(param ...)
                   (λ (this-arg bind-args)
                     (let ([local-env (new-declarative-environment scope-env)])
                       (bind-args local-env)
                       (let/ec escape
                         (syntax-parameterize
                             ([this-binding (make-rename-transformer #'this-arg)]
                              [return-binding (make-rename-transformer #'escape)])
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
  (new native-constructor%
       [prototype function:prototype]
       [call-proc call-proc]
       [formal-parameters 'TODO]
       [construct-proc construct-proc]))

(define (typical-arity proc)
  (let ([arity (procedure-arity proc)])
    (cond
      [(list? arity)
       (argmax typical-arity arity)]
      [(arity-at-least? arity)
       (add1 (arity-at-least-value arity))]
      [else arity])))

(define (make-native-function proc)
  (define arity
    (sub1 (typical-arity proc)))
  (define wrapper
    (λ (this . args)
      (if (procedure-arity-includes? proc (add1 (length args)))
          (apply proc this args)
      (apply
       proc
       this
       (for/list ([i (in-range arity)]
                  [arg (in-sequences args (in-cycle '(undefined)))])
         arg)))))
  (define f
    (new native-function%
         [prototype function:prototype]
         [call-proc wrapper]
         [formal-parameters 'TODO]))
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
