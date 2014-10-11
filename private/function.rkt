#lang racket/base

(require (for-syntax racket/base)
         racket/class
         racket/stxparam
         "environment.rkt"
         "object.rkt")

(provide function%
         constructor%
         activation%
         this
         return
         function
         function-prototype-object
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

    #;(define/public (has-instance? v)
      'undefined)))

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

(define function-prototype-object
  (new function%
       [prototype object-prototype-object]
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
                 [prot (if (object? prot) prot object-prototype-object #|FIXME|#)]
                 [obj (new ecma-object%
                           [class (get-field class prot)]
                           [prototype prot])]
                 [r (apply (get-field call-proc f) obj args)])
            (if (object? r) r obj)))]
       [f (new constructor%
               [prototype function-prototype-object]
               [call-proc proc]
               [construct-proc construct])])
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
       [prototype function-prototype-object]
       [call-proc call-proc]
       [construct-proc construct-proc]))

(define (make-native-function proc)
  (new function%
       [prototype function-prototype-object]
       [call-proc proc]))

(define-syntax-rule (native-method args body0 body ...)
  (make-data-property
   (make-native-function
    (λ args body0 body ...))))

(void
 (send function-prototype-object
       put!
       "toString"
       (make-native-function
        (λ (this)
          "TODO (function)"))))
