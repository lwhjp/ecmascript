#lang racket/base

(require racket/class
         racket/function
         racket/lazy-require
         racket/match
         racket/string
         "../private/error.rkt"
         "../private/function.rkt"
         "../private/object.rkt"
         "../private/primitive.rkt"
         "../private/this.rkt"
         (only-in "object.rkt" Object% Object:prototype))

(lazy-require
 ["../convert.rkt" (to-string to-uint32)]
 ["../private/eval.rkt" (eval)])

(provide get-properties
 Function%
 make-native-function)

(define (get-properties)
  `(["Function" . ,function-constructor]))

(define Function%
  (class ecma-function%
    (init [prototype Function:prototype])
    (super-new [prototype prototype])
    (inherit call get)
    (define/override (create-arguments-object args env)
      (new arguments%
           [prototype Object:prototype]
           [func this]
           [args args]
           [env env]))
    (define/override (construct args)
      (let* ([prot (get "prototype")]
             [prot (if (Object? prot) prot Object:prototype)]
             [obj (new Object% [prototype prot])]
             [result (call obj args)])
        (if (Object? result)
            result
            obj)))
    (let ([proto (new Object%)])
      (send proto define-own-property
        "constructor"
        `(data
          (value . ,this)
          (writable . #t)
          (enumerable . #f)
          (configurable . #t))
        #f)
      (send this define-own-property
        "prototype"
        `(data
          (value . ,proto)
          (writable . #t)
          (enumerable . #f)
          (configurable . #f))
        #f))))

(define Function:prototype
  (new Function%
       [prototype Object:prototype]
       [formal-parameters '()]
       [proc (const ecma:undefined)]))

(define (make-native-function proc)
  (define typical-arity
    (let loop ([arity (procedure-arity proc)])
      (cond
        [(list? arity) (apply max (map loop arity))]
        [(arity-at-least? arity) (arity-at-least-value arity)]
        [else arity])))
  (define wrapper
    (λ args
      (if (procedure-arity-includes? proc (length args))
          (apply proc args)
          (apply
           proc
           (for/list ([i (in-range typical-arity)]
                      [arg (in-sequences args (in-cycle (list ecma:undefined)))])
             arg)))))
  (new Function%
       [formal-parameters (build-list typical-arity (λ (i) (gensym 'arg)))]
       [proc wrapper]))

(define function-constructor
  (make-native-function
   (λ args
     (define-values (params body)
       (match args
         [(list) (values "" "")]
         [(list ps ... body)
          (values (string-join (map to-string ps) ",")
                  (to-string body))]))
     (let ([def (format "function(~a){~a};" params body)])
       (displayln def)
       (eval def)))))

(define (check-is-function o)
  (unless (Function? o)
    (raise-native-error 'type "not a function")))

(define-object-properties function-constructor
  ["prototype" Function:prototype])

(define-object-properties Function:prototype
  ["constructor" function-constructor]
  ["toString"
   (make-native-function
    (λ ()
      (check-is-function ecma:this)
      "function"))]
  ["apply"
   (make-native-function
    (λ (this-arg arg-array)
      (check-is-function ecma:this)
      (define length
        (if (Object? arg-array)
            (get-property-value arg-array "length")
            0))
      (define args
        (for/list ([i (in-range (to-uint32 length))])
          (get-property-value arg-array (to-string i))))
      (send ecma:this call this-arg args)))]
  ["call"
   (make-native-function
    (λ (this-arg . args)
      (check-is-function ecma:this)
      (send ecma:this call this-arg args)))]
  ; TODO: bind
  )
