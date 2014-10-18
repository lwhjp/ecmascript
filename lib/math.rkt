#lang racket/base

(require racket/class
         "../private/object.rkt")

(provide get-properties)

(define (get-properties)
  `(["Math" . ,math]))

(define math
  (new ecma-object%
    [prototype object-prototype]
    [class "Math"]
    [initial-properties
     `(#| TODO |#)]))
