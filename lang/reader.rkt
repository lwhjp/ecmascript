#lang s-exp syntax/module-reader
ecmascript
#:read es-read
#:read-syntax es-read-syntax
#:whole-body-readers? #t

(require "../parse.rkt"
         "../private/compile.rkt")

(define (es-read in)
  (syntax->datum
   (es-read-syntax #f in)))

(define (es-read-syntax src in)
  (displayln "reading ecma syntax")
  (define stx
    (parse-ecmascript in))
  (displayln "compiling ecma")
  (define compiled
    (ecmascript->racket stx))
  compiled)
