#lang typed/racket/base

(require (for-syntax racket/base
                     racket/string))

(provide lazy-require/typed)

(define-syntax (lazy-require/typed stx)
  (syntax-case stx ()
    [(_ [mod ([id type] ...)]
        ...)
     (with-syntax ([lazy-mod (string->symbol
                              (string-join
                               (cons
                                "lazy-require"
                                (map symbol->string (syntax->datum #'(id ... ...))))
                               "-"))])
       #'(begin
           (module lazy-mod racket/base
             (require racket/lazy-require)
             (lazy-require
              [mod (id ...)]
              ...)
             (provide id ... ...))
           (require/typed
            (submod "." lazy-mod)
            [id type]
            ... ...)))]))
