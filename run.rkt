#lang racket/base

(require racket/cmdline
         racket/port
         (prefix-in ecma: "eval.rkt"))

(provide main)

(define (main file)
  (ecma:eval
   (call-with-input-file file
     port->string)))
