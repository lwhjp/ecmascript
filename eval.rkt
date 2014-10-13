#lang racket/base

(require (prefix-in ecma:
           (combine-in
            "ast.rkt"
            "private/function.rkt"
            "private/global-object.rkt"
            "private/lang.rkt"
            "private/object.rkt"))
         racket/class
         racket/port
         racket/runtime-path
         racket/contract/base
         "parse.rkt"
         "private/compile.rkt")

(provide/contract
 (rename ecma:eval eval
         (->* (string?)
              ((is-a?/c ecma:ecma-object%)
               namespace?)
              any))
 [make-global-namespace (-> namespace?)])

(define (ecma:eval prog
                   [scope ecma:global-object]
                   [namespace (make-global-namespace)])
  (eval
   #`(begin
      #,@(ecmascript->racket
          (call-with-input-string prog
            read-program)))
   namespace))

(define-namespace-anchor here)
(define-runtime-module-path-index lang-module "private/lang.rkt")

(define (make-global-namespace)
  (parameterize
      ([current-namespace
        (namespace-anchor->empty-namespace here)])
    (namespace-require lang-module)
    (current-namespace)))

(void
 (send
  ecma:global-object
  put!
  "eval"
  (ecma:make-native-function
   (λ (this x)
     (if (string? x)
         (ecma:eval x) ; FIXME: nested lexical scopes
         x)))))
