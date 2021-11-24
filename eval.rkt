#lang racket/base

(require racket/class
         racket/contract/base
         racket/port
         (rename-in "private/eval.rkt"
                    [eval do-eval])
         "private/default-environment.rkt"
         "private/error.rkt"
         "private/init.rkt"
         "private/realm.rkt")

(provide
 (contract-out
  [realm? predicate/c]
  [current-realm (parameter/c realm?)]
  [make-realm (-> realm?)]
  [es-eval (->* ((or/c string? input-port? path?)) (realm?) any)]))

(define (realm? v) (is-a? v realm%))

(define (es-eval src
                 [realm (current-realm)])
  (with-es-exceptions
    (if (path? src)
        (call-with-input-file src
          (λ (in)
            (port-count-lines! in)
            (do-eval in realm)))
        (do-eval src realm))))
