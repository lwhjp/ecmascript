#lang racket/base

(require racket/class
         "object.rkt")

(provide global-object)

; FIXME: this should be parameterized so that we can create
; a new JS environment without reloading modules (copy the
; "default" global-object containing library functions)

(define global-object
  (new ecma-object%
       [class-name 'global]
       [prototype #f]))
