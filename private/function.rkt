#lang racket/base

(require (for-syntax racket/base)
         racket/class
         racket/stxparam
         "environment.rkt"
         "object.rkt")

(provide function%
         constructor%
         activation%
         return
         function
         function-prototype
         make-native-constructor
         make-native-function
         native-method)

(define-syntax-parameter return
  (λ (stx)
    (raise-syntax-error #f "invalid outside of function scope" stx)))

(define function%
  (class ecma-object%
    (init-field call-proc
                #;scope
                #;formal-parameters
                #;code)

    (super-new [class "Function"])

    (define/public (call this . args)
      (apply call-proc this args))

    (define/public (has-instance? v)
      (and
       (is-a? v ecma-object%)
       (let ([o (send this get "prototype")])
         (unless (is-a? o ecma-object%)
           (error "type error"))
         (let loop ([v v])
           (let ([v (get-field prototype v)])
             (and
              v
              (or (eq? o v)
                  (loop v))))))))))

(define constructor%
  (class function%
    (init-field construct-proc)

    (super-new)

    (define/public (construct . args)
      (apply construct-proc args))))

(define activation%
  (class ecma-object%
    (super-new [prototype #f]
               [class "Object"])))

(define function-prototype
  (new function%
       [prototype object-prototype]
       [call-proc (λ args
                    'undefined)]))

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

(define (make-function-object proc)
  (letrec
      ([construct
        (λ args
          (let* ([prot (send f get "prototype")]
                 [prot (if (is-a? prot ecma-object%) prot object-prototype #|FIXME|#)]
                 [obj (new ecma-object%
                           [class (get-field class prot)]
                           [prototype prot])]
                 [r (apply (get-field call-proc f) obj args)])
            (if (is-a? r ecma-object%) r obj)))]
       [f (new constructor%
               [prototype function-prototype]
               [call-proc proc]
               [construct-proc construct])])
    ; TODO: length
    (let ([proto
           (new ecma-object%
                [prototype object-prototype]
                [class "Object"])])
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
            #f))
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

(define-syntax-rule (function defined-args body0 body ...)
  (letrec
      ([f (make-function-object
           (λ (this-arg . args)
             (let ([activation (make-activation-object f args)])
               (create-arguments! activation 'defined-args args)
               (begin-scope activation
                 (let/ec escape
                   (syntax-parameterize
                    ([this-binding (make-rename-transformer #'this-arg)]
                     [return (λ (stx)
                               (syntax-case stx ()
                                 [(_) #'(escape 'undefined)]
                                 [(_ v) #'(escape v)]))])
                    body0 body ...))))))])
     f))

(define (make-native-constructor call-proc construct-proc)
  (new constructor%
       [prototype function-prototype]
       [call-proc call-proc]
       [construct-proc construct-proc]))

(define (make-native-function proc)
  (new function%
       [prototype function-prototype]
       [call-proc proc]))

(define-syntax-rule (native-method args body0 body ...)
  (make-native-function
   (λ args body0 body ...)))
