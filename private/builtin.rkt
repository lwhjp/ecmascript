#lang racket/base

(require racket/class
         "object.rkt")

(provide (all-defined-out))

(define boolean%
  (class ecma-object%
    (init-field value)
    (super-new [class "Boolean"])))

(define number%
  (class ecma-object%
    (init-field value)
    (super-new [class "Number"])))

(define regexp%
  (class ecma-object%
    (init-field pattern flags)
    (super-new [class "RegExp"])))

(define string%
  (class ecma-object%
    (init-field value)
    (super-new [class "String"]
               [initial-properties
                `(("length" . ,(make-data-property (string-length value))))])))
