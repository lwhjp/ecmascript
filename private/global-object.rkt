#lang racket/base

(require "object.rkt")

(provide global-object)

(define global-object
  (Object #f (make-hash) #t))
