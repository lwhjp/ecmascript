#lang racket/base

(require "eval.rkt"
         "private/lang.rkt")

(provide (all-from-out "private/lang.rkt")

         (rename-out
          [ecma:module-begin #%module-begin]
          [ecma:top-interaction #%top-interaction]))

(define-syntax-rule (ecma:module-begin form ...)
  (#%module-begin
   form ...))

(define-syntax-rule (ecma:top-interaction . form)
  (get-value form))

(module main racket/base
  (require racket/cmdline
           racket/port
           (prefix-in ecma: "eval.rkt"))
  (command-line
   #:args (file)
   (ecma:eval
    (call-with-input-file file
      port->string))))
