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
  (define stx
    (parse-ecmascript in))
  (define compiled
    (ecmascript->racket stx))
  compiled)

(current-read-interaction
 (λ (src in)
   (let ([line (read-line in)])
     (if (eof-object? line)
         line
         #`(begin
             #,@(es-read-syntax src (open-input-string line)))))))
