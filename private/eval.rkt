#lang racket/base

(require racket/class
         racket/port
         racket/promise
         racket/runtime-path
         racket/contract/base
         "realm.rkt"
         "../lang/compile.rkt"
         "../lang/read.rkt"
         "../parse.rkt")

(provide (contract-out
          (rename ecma:eval eval
                  (->* ((or/c string? input-port?))
                       ((is-a?/c realm%))
                       any)))
         eval-read-interaction)

(define (ecma:eval src
                   [realm (current-realm)])
  (define stx
    (cond
      [(string? src) (with-input-from-string src ecma:read-syntax)]
      [(input-port? src) (parameterize ([current-input-port src])
                           (ecma:read-syntax))]))
  (if (eof-object? stx)
      (void)
      (parameterize ([current-realm realm])
        (eval
         #`(begin #,@stx)
         (force es-eval-namespace)))))

(define-namespace-anchor here)

(define es-eval-namespace
  (lazy
   (let ([ns (namespace-anchor->empty-namespace here)])
     (namespace-require 'ecmascript/lang/main ns)
     ns)))

(define (eval-read-interaction src in)
  (let ([line (read-line in)])
    (if (eof-object? line)
        line
        #`(begin
            #,@(ecmascript->racket
                (read-program src (open-input-string line)))))))
