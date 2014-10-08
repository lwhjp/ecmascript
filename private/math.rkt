#lang racket/base

(require "object.rkt")

(provide math-object)

(define math-object
  (object
   (object-class object-prototype-object)
   object-prototype-object
   (make-hash
    #| TODO |#)))
