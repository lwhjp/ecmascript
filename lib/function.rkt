#lang racket/base

(require racket/match
         racket/string
         "../private/function.rkt"
         "../private/object.rkt"
         "../eval.rkt"
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

(define-object-properties function-constructor
  ["prototype" function-prototype])

(define-object-properties function-prototype
  ["constructor" function-constructor])
