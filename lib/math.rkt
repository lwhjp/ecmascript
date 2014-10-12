#lang racket/base

(require racket/class
         "../private/object.rkt")

(provide (all-defined-out))

(define math
  (new ecma-object%
    [prototype object-prototype]
    [class "Math"]
    [initial-properties
     `(#| TODO |#)]))
