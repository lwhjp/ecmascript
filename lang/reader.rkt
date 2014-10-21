#lang s-exp syntax/module-reader
ecmascript
#:read es-read
#:read-syntax es-read-syntax
#:whole-body-readers? #t

(require "../parse.rkt"
         "../private/compile.rkt")

(define (es-read in)
  (let ([stx (es-read-syntax #f in)])
    (if (eof-object? stx)
        eof
        (syntax->datum stx))))

(define (es-read-syntax src in)
  (let ([ast (read-program src in)])
    (if (eof-object? ast)
        eof
        (ecmascript->racket ast))))
