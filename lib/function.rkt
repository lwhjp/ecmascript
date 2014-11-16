#lang racket/base

(require (only-in racket/class is-a? send)
         racket/match
         racket/string
         "../private/error.rkt"
         "../private/function.rkt"
         "../private/object.rkt"
         "../convert.rkt"
         "../eval.rkt"
         "../object.rkt"
         "../types.rkt")

(provide get-properties)

(define (get-properties)
  `(["Function" . ,function-constructor]))

(define function-constructor
  (letrec
      ([call
        (λ (this . args)
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
  (unless (is-a? o function%)
    (raise-native-error 'type "not a function")))

(define-object-properties function-constructor
  ["prototype" function-prototype])

(define-object-properties function-prototype
  ["constructor" function-constructor]
  ["toString"
   (make-native-function
    (λ (this)
      (check-is-function this)
      "function"))]
  ["apply"
   (make-native-function
    (λ (this this-arg arg-array)
      (check-is-function this)
      (define length
        (if (is-a? arg-array ecma-object%)
            (get arg-array "length")
            0))
      (define args
        (for/list ([i (in-range (to-uint32 length))])
          (get arg-array (to-string i))))
      (send this call this-arg . arg-array)))]
  ["call"
   (make-native-function
    (λ (this this-arg . args)
      (check-is-function this)
      (send this call this-arg . args)))]
  ; TODO: bind
  )
