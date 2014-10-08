#lang racket/base

(require racket/stxparam
         "object.rkt"
         "scope.rkt"
         (for-syntax racket/base))

(provide (struct-out function-object)
         (struct-out constructor-object)
         (struct-out activation-object)
         this
         return
         function
         function-prototype-object
         make-native-constructor
         make-native-function
         native-method)

(define-syntax-parameter this
  (λ (stx)
    (raise-syntax-error #f "invalid outside of function scope" stx)))

(define-syntax-parameter return
  (λ (stx)
    (raise-syntax-error #f "invalid outside of function scope" stx)))

(struct function-object object (proc))

(struct constructor-object function-object (proc))

(define function-prototype-object
  (function-object
   "Function"
   object-prototype-object
   (make-hash)
   (λ args
     'undefined)))

(struct activation-object object ())

(define (make-activation-object f args)
  (activation-object
   (object-class object-prototype-object)
   object-prototype-object
   (make-hash
    `(("arguments" . ,(make-arguments-object f args))))))

(define (make-arguments-object f args)
  (object
   (object-class object-prototype-object)
   object-prototype-object
   (make-hash
    `(("callee" . ,(property f #f #t #f))
      ("length" . ,(property (length args) #f #t #f))
      ,@(for/list ([n (in-naturals)]
                   [arg (in-list args)])
          (cons (number->string n)
                (property arg #f #t #f)))))))

(define (make-function-object proc)
  (letrec
      ([construct
        (λ args
          (let* ([prot (get f "prototype")]
                 [prot (if (object? prot) prot object-prototype-object #|FIXME|#)]
                 [obj (object
                       (object-class prot)
                       prot
                       (make-hash))]
                 [r (apply (function-object-proc f) obj args)])
            (if (object? r) r obj)))]
       [f (constructor-object
           (object-class function-prototype-object)
           function-prototype-object
           (make-hash)
           proc
           construct)])
    f))

(define (create-arguments! obj args vals)
  ;; Use the last value (possibly not supplied) for repeated argument names
  (let ([arg-map (make-immutable-hasheq
                  (for/list ([arg (in-list args)]
                             [val (in-sequences vals
                                                (in-cycle '(undefined)))])
                       (cons arg val)))])
    (create-variables! obj (hash->list arg-map))))

(define-syntax-rule (function defined-args body0 body ...)
  (letrec
      ([f (make-function-object
           (λ (this-arg . args)
             (let ([activation (make-activation-object f args)])
               (create-arguments! activation 'defined-args args)
               (begin-scope activation
                 (let/ec escape
                   (syntax-parameterize
                    ([this (make-rename-transformer #'this-arg)]
                     [return (λ (stx)
                               (syntax-case stx ()
                                 [(_) #'(escape 'undefined)]
                                 [(_ v) #'(escape v)]))])
                    body0 body ...))))))])
     f))

(define (make-native-constructor call-proc new-proc)
  (constructor-object
   (object-class function-prototype-object)
   function-prototype-object
   (make-hash)
   call-proc
   new-proc))

(define (make-native-function proc)
  (function-object
   (object-class function-prototype-object)
   function-prototype-object
   (make-hash)
   proc))

(define-syntax-rule (native-method args body0 body ...)
  (make-property
   (make-native-function
    (λ args body0 body ...))))

(put! function-prototype-object
      "toString"
      (make-native-function
       (λ (this)
         "TODO (function)")))
