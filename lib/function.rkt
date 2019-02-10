#lang racket/base

(require (only-in racket/class get-field)
         racket/match
         racket/string
         "../private/error.rkt"
         "../private/function.rkt"
         "../private/object.rkt"
         "../private/this.rkt"
         "../convert.rkt"
         "../eval.rkt"
         "../object.rkt")

(provide get-properties)

(define (get-properties)
  `(["Function" . ,function-constructor]))

(define function-constructor
  (letrec
      ([call
        (λ args
          (apply construct args))]
       [construct
        (λ args
          (define-values (params body)
            (match args
              [(list) (values "" "")]
              [(list ps ... body)
               (values (string-join (map to-string ps) ",")
                       (to-string body))]))
          (let ([def (format "function(~a){~a};" params body)])
            (displayln def)
            (eval def)))])
    (make-native-constructor call construct)))

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
      (apply/this this-arg (get-field proc ecma:this) args)))]
  ["call"
   (make-native-function
    (λ (this-arg . args)
      (check-is-function ecma:this)
      (apply/this this-arg (get-field proc ecma:this) args)))]
  ; TODO: bind
  )
