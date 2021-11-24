#lang racket/base

(require (for-syntax racket/base
                     racket/syntax)
         racket/class
         racket/lazy-require
         "../private/error.rkt"
         "../private/object.rkt"
         "../private/primitive.rkt"
         "../private/this.rkt"
         (only-in "object.rkt" Object:prototype)
         "util.rkt")

(lazy-require
 ["../lang/statement.rkt" (throw)]
 ["../convert.rkt" (to-string)])

(provide get-properties)

(define (get-properties)
  `(["Error" . ,error-constructor]
    ["EvalError" . ,eval-error-constructor]
    ["RangeError" . ,range-error-constructor]
    ["ReferenceError" . ,reference-error-constructor]
    ["SyntaxError" . ,syntax-error-constructor]
    ["TypeError" . ,type-error-constructor]
    ["URIError" . ,uri-error-constructor]))

(define Error%
  (class ecma-object%
    (super-new [class-name 'Error])))

(define (make-error-prototype+constructor name super-prototype)
  (letrec
      ([prototype
        (new Error% [prototype super-prototype])]
       [constructor
        (make-native-function
         (λ ([message ecma:undefined])
           (new Error%
                [prototype prototype]
                [properties (make-hash
                             (if (ecma:undefined? message)
                                 '()
                                 `(("message" . ,(make-data-property
                                                  (to-string message))))))])))])
    (define-object-properties prototype
      ["constructor" constructor]
      ["name" name]
      ["message" ""])
    (define-object-properties constructor
      ["prototype" prototype])
    (values prototype constructor)))

(define-values (error:prototype error-constructor)
  (make-error-prototype+constructor "Error" Object:prototype))

(define-syntax (define-native-error stx)
  (syntax-case stx ()
    [(_ base-str)
     (let ([base-name (string->symbol
                       (string-append
                        (string-downcase
                         (syntax-e #'base-str))
                        "-error"))])
       (with-syntax
         ([proto-id (format-id stx "~a:prototype" base-name)]
          [cons-id (format-id stx "~a-constructor" base-name)]
          [make-id (format-id stx "make-~a" base-name)])
         #'(define-values (proto-id cons-id make-id)
             (let-values
                 ([(proto cons)
                   (make-error-prototype+constructor
                    (string-append base-str "Error")
                    error:prototype)])
               (values proto
                       cons
                       (λ (msg) (send cons-id construct (list msg))))))))]))

(define-native-error "Eval")
(define-native-error "Range")
(define-native-error "Reference")
(define-native-error "Syntax")
(define-native-error "Type")
(define-native-error "URI")

(define-object-properties error:prototype
  ["toString"
   (make-native-function
    (λ ()
      (unless (Object? ecma:this)
        (raise-native-error 'type "this: not an object"))
      (define name
        (let ([name (get-property-value ecma:this "name")])
          (if (ecma:undefined? name)
              "Error"
              (to-string name))))
      (define msg
        (let ([msg (get-property-value ecma:this "message")])
          (if (ecma:undefined? msg)
              ""
              (to-string msg))))
      (cond
        [(string=? "" name) msg]
        [(string=? "" msg) name]
        [else (string-append name ": " msg)])))])

(native-error-constructor
 (λ (type message)
   (case type
     [(range) (make-range-error message)]
     [(reference) (make-reference-error message)]
     [(syntax) (make-syntax-error message)]
     [(type) (make-type-error message)]
     [(uri) (make-uri-error message)]
     [else
      (error 'native-error-handler
             "unknown error type: ~a"
             type)])))
