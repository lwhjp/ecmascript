#lang racket/base

(require racket/class
         "object.rkt")

(provide (all-defined-out))

(define boolean%
  (class ecma-object%
    (init-field value)
    (super-new [class "Boolean"])))

(define boolean-prototype
  (instantiate boolean% (#f)
    [prototype object-prototype]))

(define number%
  (class ecma-object%
    (init-field value)
    (super-new [class "Number"])))

(define number-prototype
  (instantiate number% (0)
    [prototype object-prototype]))

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

(define string-prototype
  (instantiate string% ("")
    [prototype object-prototype]))
