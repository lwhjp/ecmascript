#lang racket/base

(require racket/class
         "object.rkt")

(provide (all-defined-out))

(define array%
  (class ecma-object%
    (super-new
     [class "Array"]
     [initial-properties
      `(("length" . ,(make-data-property 0
                       #:writable #t
                       #:configurable #f
                       #:enumerable #f)))])))

(define array-prototype
  (new array%
       [prototype object-prototype]))
