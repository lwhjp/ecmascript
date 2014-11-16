#lang racket/base

(require (prefix-in ecma:
           (combine-in
            "private/function.rkt"
            "private/global-object.rkt"
            "private/object.rkt"))
         racket/port
         racket/runtime-path
         racket/contract/base
         "object.rkt"
         "parse.rkt"
         "private/compile.rkt"
         "private/read.rkt")

(provide (contract-out
          (rename ecma:eval eval
                  (->* (string?)
                       (object? namespace?)
                       any))
          [make-global-namespace (-> namespace?)])
         eval-read-interaction)

(define (ecma:eval prog
                   [scope ecma:global-object]
                   [namespace (make-global-namespace)])
  (let ([stx (with-input-from-string prog
               (λ ()
                 (ecma:read-syntax)))])
    (if (eof-object? stx)
        (void)
        (eval
         #`(begin #,@stx)
         namespace))))

(define-namespace-anchor here)
(define-runtime-module-path-index main-module "main.rkt")

(define (make-global-namespace)
  (parameterize
      ([current-namespace
        (namespace-anchor->empty-namespace here)])
    (namespace-require main-module)
    (current-namespace)))

(define (eval-read-interaction src in)
  (let ([line (read-line in)])
    (if (eof-object? line)
        line
        #`(begin
            #,@(ecmascript->racket
                (read-program src (open-input-string line)))))))
