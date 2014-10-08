#lang racket/base

(require "function.rkt"
         "object.rkt")

(provide (all-defined-out))

(struct boolean-object object (value))

(define boolean-prototype-object
  (boolean-object
   "Boolean"
   object-prototype-object
   (make-hash
    `(("toString" . ,(native-method (this)
                       (if (boolean-object-value this)
                           "true"
                           "false")))
      ("valueOf" . ,(native-method (this)
                      (boolean-object-value this)))))
   #f))

(define (make-boolean-object v)
  (unless (boolean? v)
    (raise-argument-error 'make-boolean-object "boolean?" v))
  (boolean-object
   (object-class boolean-prototype-object)
   boolean-prototype-object
   (make-hash)
   v))
