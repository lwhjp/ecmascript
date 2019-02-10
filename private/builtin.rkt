#lang racket/base

(require racket/class
         "object.rkt")

(provide (all-defined-out))

(define Boolean%
  (class ecma-object%
    (init [prototype Boolean:prototype])
    (init-field [value #f])
    (super-new [class-name 'Boolean]
               [prototype prototype])))

(define Boolean:prototype
  (new Boolean% [prototype Object:prototype]))

(define (make-Boolean value)
  (new Boolean% [value value]))

(define Number%
  (class ecma-object%
    (init [prototype Number:prototype])
    (init-field [value 0])
    (super-new [class-name 'Number]
               [prototype prototype])))

(define Number:prototype
  (new Number% [prototype Object:prototype]))

(define (make-Number value)
  (new Number% [value value]))

(define RegExp%
  (class ecma-object%
    (init prototype)
    (init-field pattern flags)
    (super-new [class-name 'RegExp]
               [prototype prototype])))

(define String%
  (class ecma-object%
    (init [prototype String:prototype])
    (init-field [value ""])
    (super-new [class-name 'String]
               [prototype prototype])))

(define String:prototype
  (new String% [prototype Object:prototype]))

(define (make-String value)
  (new String% [value value]))
