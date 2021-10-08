#lang racket/base

(require racket/class
         racket/contract/base
         racket/port
         (rename-in "private/eval.rkt"
                    [eval do-eval])
         "private/default-environment.rkt"
         "private/init.rkt"
         "private/realm.rkt")

(provide
 (contract-out
  [realm? predicate/c]
  [current-realm (parameter/c realm?)]
  [make-realm (-> realm?)]
  [es-eval (->* ((or/c string? input-port? path?)) (realm?) any/c)]))

(define (realm? v) (is-a? v realm%))

(define (es-eval src
                 [realm (current-realm)])
  (do-eval
   (cond
     [(string? src) src]
     [(input-port? src) (port->string src)]
     [(path? src) (call-with-input-file src port->string)])
   realm))
