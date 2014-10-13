#lang racket/base

(require racket/class
         "boolean.rkt"
         "function.rkt"
         "math.rkt"
         "object.rkt"
         "string.rkt"
         "../private/global-object.rkt"
         "../private/function.rkt"
         "../private/object.rkt"
         "../private/types.rkt")

(provide)

;; TODO: array

(define-object-properties global-object
  ["NaN" +nan.0]
  ["Infinity" +inf.0]
  ; TODO: eval
  ; TODO: parseInt
  ; TODO: parseFloat
  ; TODO: escape
  ; TODO: unescape
  ; TODO: isNaN
  ; TODO: isFinite
  ["Object" object-constructor]
  ["Function" function-constructor]
  ; ["Array" array-constructor]
  ["String" string-constructor]
  ["Boolean" boolean-constructor]
  ; ["Number" number-constructor]
  ; ["Date" date-constructor]
  ["Math" math])
