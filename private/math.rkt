#lang racket/base

(require racket/class
         "object.rkt")

(provide math-object)

(define math-object
  (new ecma-object%
    [prototype object-prototype-object]
    [class "Math"]
    [initial-properties
     `(#| TODO |#)]))
