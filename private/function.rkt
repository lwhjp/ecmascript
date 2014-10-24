#lang racket/base

(require (for-syntax racket/base
                     syntax/parse)
         racket/class
         racket/list
         racket/stxparam
         "environment.rkt"
         "error.rkt"
         "object.rkt")

(provide function%
         constructor%
         activation%
         function-prototype
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
      (let ([activation (make-activation-object this args)])
        (create-arguments! activation formal-parameters args)
        (call-proc this-arg activation)))

    (define/public (has-instance? v)
      (and
       (is-a? v ecma-object%)
       (let ([o (send this get "prototype")])
         (unless (is-a? o ecma-object%)
           (raise-native-error 'type "not an object"))
         (let loop ([v v])
           (let ([v (get-field prototype v)])
             (and
              v
              (or (eq? o v)
                  (loop v))))))))))

(define constructor%
  (class function%
    (init-field)

    (super-new)

    (define/public (construct . args)
      (let* ([prot (send this get "prototype")]
             [prot (if (object? prot) prot object-prototype)]
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

(define function-prototype
  (new function%
       [prototype object-prototype]
       [call-proc (λ args
                    'undefined)]
       [formal-parameters '()]))

(define (make-activation-object f args)
  (new activation%
       [initial-properties
        `(("arguments" . ,(make-data-property
                           (make-arguments-object f args))))]))

(define (make-arguments-object f args)
  (new ecma-object%
       [prototype #f]
       [class "Object"]
       [initial-properties
        `(("callee" . ,(make-data-property f))
          ("length" . ,(make-data-property (length args)))
          ,@(for/list ([n (in-naturals)]
                       [arg (in-list args)])
              (cons (number->string n)
                    (make-data-property arg))))]))

(define (make-function params proc)
  (let ([f (new constructor%
                [prototype function-prototype]
                [call-proc proc]
                [formal-parameters params])]
        [proto
           (new ecma-object%
                [prototype object-prototype]
                [class "Object"])])
    (send f define-own-property
          "length"
          `(data
            (value . ,(length params))
            (writable . #f)
            (enumerable . #f)
            (configurable . #f))
          #f)
    (send proto define-own-property
          "constructor"
          `(data
            (value . ,f)
            (writable . #t)
            (enumerable . #f)
            (configurable . #t))
          #f)
    (send f define-own-property
          "prototype"
          `(data
            (value . ,proto)
            (writable . #t)
            (enumerable . #f)
            (configurable . #f))
          #f)
    f))

(define (create-arguments! obj args vals)
  ;; Use the last value (possibly not supplied) for repeated argument names
  (let ([arg-map (make-immutable-hasheq
                  (for/list ([arg (in-list args)]
                             [val (in-sequences vals
                                                (in-cycle '(undefined)))])
                       (cons arg val)))])
    (for ([(id val) (in-hash arg-map)])
      (send obj put! (symbol->string id) val))))

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
   [(_ (~! param:id ...)
       (~optional (~seq #:vars (var-id:id ...)))
       body:expr ...+)
    #`(make-function '(param ...)
        (λ (this-arg activation)
          (begin-scope activation
            #,@(if (attribute var-id) #'(#:vars (var-id ...)) #'())
            (let/ec escape
              (syntax-parameterize
                  ([this-binding (make-rename-transformer #'this-arg)]
                   [return-binding (make-rename-transformer #'escape)])
                body ...)))))]))

(define-syntax (begin-scope stx)
  (syntax-parse stx
    [(_ scope-obj (~optional (~seq #:vars (var-id:id ...))) form ...)
     #'(let ([new-scope (new-object-environment
                         scope-obj
                         lexical-environment)])
         (syntax-parameterize
             ([variable-environment (make-rename-transformer #'new-scope)]
              [lexical-environment (make-rename-transformer #'new-scope)])
           (reorder-functions () ()
             (create-variables! variable-environment '(var-id ...))
             form ...)))]))

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
       [prototype function-prototype]
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
         [prototype function-prototype]
         [call-proc wrapper]
         [formal-parameters 'TODO]))
  (send f define-own-property
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
