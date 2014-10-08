#lang s-exp syntax/module-reader
ecmascript
#:read es-read
#:read-syntax es-read-syntax
#:whole-body-readers? #t

(require "../private/compile.rkt"
         "../private/parse.rkt")

(define (es-read in)
  (syntax->datum
   (es-read-syntax #f in)))

(define (es-read-syntax src in)
  (displayln "reading ecma syntax")
  (define stx (es-parse in))
  (displayln "compiling ecma")
  (define compiled (es-compile stx))
  compiled)
