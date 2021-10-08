#lang racket/base

(require racket/cmdline
         racket/port
         "eval.rkt")

(provide main)

(define (main file)
  (es-eval
   (call-with-input-file file
     port->string)
   (make-realm)))
