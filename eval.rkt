#lang racket/base

(require racket/class
         racket/contract/base
         racket/port
         (rename-in "private/eval.rkt"
                    [eval do-eval])
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
  (unless (is-a? realm realm%)
    (error 'es-eval "a realm was not provided and (current-realm) has not been set"))
  (do-eval
   (cond
     [(string? src) src]
     [(input-port? src) (port->string src)]
     [(path? src) (call-with-input-file src port->string)])
   realm))
