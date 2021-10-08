#lang racket/base

(require racket/class
         racket/port
         racket/promise
         racket/runtime-path
         racket/contract/base
         "private/realm.rkt"
         "lang/compile.rkt"
         "lang/read.rkt"
         "object.rkt"
         "parse.rkt")

(provide (contract-out
          (rename ecma:eval eval
                  (->* (string?)
                       ((is-a?/c realm%))
                       any)))
         eval-read-interaction)

(define (ecma:eval prog
                   [realm (current-realm)])
  (let ([stx (with-input-from-string prog
               (λ ()
                 (ecma:read-syntax)))])
    (if (eof-object? stx)
        (void)
        (parameterize ([current-realm realm])
          (eval
           #`(begin #,@stx)
           (force es-eval-namespace))))))

(define-namespace-anchor here)
(define-runtime-module-path-index main-module "main.rkt")

(define es-eval-namespace
  (lazy
   (parameterize
       ([current-namespace
         (namespace-anchor->empty-namespace here)])
     (namespace-require main-module)
     (current-namespace))))

(define (eval-read-interaction src in)
  (let ([line (read-line in)])
    (if (eof-object? line)
        line
        #`(begin
            #,@(ecmascript->racket
                (read-program src (open-input-string line)))))))
