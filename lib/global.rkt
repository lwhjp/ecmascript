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
  ["NaN" (make-data-property +nan.0)]
  ["Infinity" (make-data-property +inf.0)]
  ; TODO: eval
  ; TODO: parseInt
  ; TODO: parseFloat
  ; TODO: escape
  ; TODO: unescape
  ; TODO: isNaN
  ; TODO: isFinite
  ["Object" (make-data-property object-constructor)]
  ["Function" (make-data-property function-constructor)]
  ; ("Array" (make-data-property array-constructor))
  ["String" (make-data-property string-constructor)]
  ["Boolean" (make-data-property boolean-constructor)]
  ; ("Number" . ,(make-data-property number-constructor))
  ; ("Date" . ,(make-data-property date-constructor))
  ["Math" (make-data-property math)])
