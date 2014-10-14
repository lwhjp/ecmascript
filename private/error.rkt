#lang racket/base

(require (for-syntax racket/base
                     racket/syntax)
         racket/class
         "function.rkt"
         "object.rkt"
         "types.rkt")

(provide (all-defined-out))

(define error%
  (class ecma-object%
    (super-new [class "Error"])))

(define (make-error-prototype+constructor name super-prototype)
  (let ([prototype
         (new error%
              [prototype super-prototype])]
        [constructor
         (letrec
             ([call
               (λ (this . args)
                 (apply construct args))]
              [construct
               (λ (message)
                 (new error%
                      [prototype function-prototype]
                      [initial-properties
                       `(("message" . ,(make-data-property
                                        (to-string message))))]))])
           (make-native-constructor call construct))])
    (define-object-properties prototype
      ["constructor" constructor]
      ["name" name]
      ["message" ""])
    (define-object-properties constructor
      ["prototype" prototype])
    (values prototype constructor)))

(define-values (error-prototype error-constructor)
  (make-error-prototype+constructor "Error" object-prototype))

(define-syntax (define-native-error stx)
  (syntax-case stx ()
    [(_ base-str)
     (let ([base-name (string->symbol
                       (string-append
                        (string-downcase
                         (syntax-e #'base-str))
                        "-error"))])
       (with-syntax
         ([proto-id (format-id stx "~a-prototype" base-name)]
          [cons-id (format-id stx "~a-constructor" base-name)]
          [throw-id (format-id stx "throw-~a" base-name)])
         #'(define-values (proto-id cons-id throw-id)
             (let-values
                 ([(proto cons)
                   (make-error-prototype+constructor
                    (string-append base-str "Error")
                    error-prototype)])
               (values proto
                       cons
                       (λ (msg)
                         (send cons-id new msg)))))))]))

(define-native-error "Eval")
(define-native-error "Range")
(define-native-error "Reference")
(define-native-error "Syntax")
(define-native-error "Type")
(define-native-error "URI")
