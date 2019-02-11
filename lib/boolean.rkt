#lang racket/base

(require racket/class
         racket/lazy-require
         "../private/object.rkt"
         "../private/primitive.rkt"
         "../private/this.rkt"
         (only-in "function.rkt" Function%)
         (only-in "object.rkt" Object:prototype)
         "util.rkt")

(lazy-require
 ["../convert.rkt" (to-boolean)])

(provide get-properties
 make-Boolean)

(define (get-properties)
  `(["Boolean" . ,boolean-constructor]))

(define Boolean%
  (class ecma-object%
    (init-field value)
    (init [prototype Boolean:prototype])
    (super-new [class-name 'Boolean]
               [prototype prototype])))

(define Boolean:prototype
  (new Boolean%
       [prototype Object:prototype]
       [value #f]))

(define (make-Boolean value)
  (new Boolean% [value value]))

(define boolean-constructor
  (new
   (class Function%
     (super-new [formal-parameters '(value)]
                [proc to-boolean])
     (define/override (construct args)
       (let ([value (if (null? args) ecma:undefined (car args))])
         (make-Boolean (to-boolean value)))))))

(define-object-properties boolean-constructor
  ["prototype" Boolean:prototype])

(define-object-properties Boolean:prototype
  ["constructor" boolean-constructor]
  ["toString"
   (native-method ()
     (if (get-field value ecma:this)
         "true"
         "false"))]
  ["valueOf"
   (native-method ()
     (get-field value ecma:this))])
