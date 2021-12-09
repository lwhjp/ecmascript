#lang racket/base

(require racket/contract/base
         (rename-in "private/eval.rkt"
                    [eval do-eval])
         "private/default-environment.rkt"
         "private/init.rkt"
         "private/realm.rkt")

(provide
 (contract-out
  [realm? predicate/c]
  [current-realm (parameter/c realm?)]
  [make-default-realm (-> realm?)]
  [es-eval (->* ((or/c string? input-port? path?)) (realm?) any)]))

(define (es-eval src
                 [realm (current-realm)])
  (if (path? src)
      (call-with-input-file src
        (λ (in)
          (port-count-lines! in)
          (do-eval in realm)))
      (do-eval src realm)))
