#lang racket/base

(require racket/class
         "function.rkt"
         "object.rkt")

(provide (all-defined-out))

(define boolean%
  (class ecma-object%
    (init-field value)
    (super-new [class "Boolean"])))

(define boolean-prototype-object
  (instantiate boolean% (#f)
    [prototype object-prototype-object]
    [initial-properties
     `(("toString" . ,(native-method (this)
                        (if (get-field value this)
                            "true"
                            "false")))
       ("valueOf" . ,(native-method (this)
                       (get-field value this))))]))

(define (make-boolean-object v)
  (unless (boolean? v)
    (raise-argument-error 'make-boolean-object "boolean?" v))
  (instantiate boolean% (v)
    [prototype boolean-prototype-object]))
