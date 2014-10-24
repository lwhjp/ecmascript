#lang racket/base

(require ragg/support
         "../parse.rkt"
         "../private/compile.rkt"
         "../private/error.rkt")

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
      ([(λ (e)
          (or (exn:fail:read? e)
              (exn:fail:parsing? e)))
        (λ (e)
          (raise-native-error 'syntax (exn-message e)))])
    (let ([ast (read-program src in)])
      (if (eof-object? ast)
          eof
          (ecmascript->racket ast)))))