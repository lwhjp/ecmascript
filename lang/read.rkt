#lang racket/base

(require "../private/error.rkt"
         "../parse.rkt"
         "compile.rkt")

(provide (all-defined-out))

(define (ecma:read [in (current-input-port)])
  (let ([stx (ecma:read-syntax #f in)])
    (if (eof-object? stx)
        eof
        (syntax->datum stx))))

(define (ecma:read-syntax
         [src (object-name (current-input-port))]
         [in (current-input-port)])
  (with-handlers
      ([exn:fail:read?
        (λ (e)
          (raise-native-error 'syntax (exn-message e)))])
    (let ([ast (read-program src in)])
      (if (eof-object? ast)
          '()
          (list (ecmascript->racket ast))))))
