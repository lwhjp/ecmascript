#lang racket/base

(require (for-syntax racket/base
                     syntax/parse
                     syntax/stx)
         racket/class
         "../private/environment.rkt"
         "../private/object.rkt"
         (only-in "../lib/array.rkt" make-array)
         (only-in "../lib/object.rkt" Object%)
         "../convert.rkt"
         "environment.rkt"
         (prefix-in ecma: "function.rkt"))

(provide
 (all-defined-out)
 (rename-out [make-array array]))

(define-syntax (object stx)
  (define parse-name
    (syntax-parser
      [v:id (symbol->string (syntax-e #'v))]
      [v:str #'v]
      [v:number #'(to-string v)]))
  (define parse-def
    (syntax-parser
      [(expr)
       #'`(data
           (value . ,(get-value expr))
           (writable . #t)
           (enumerable . #t)
           (configurable . #t))]
      [(#:get fn)
       #'`(accessor
           (get . ,fn)
           (enumerable . #t)
           (configurable . #t))]
      [(#:set fn)
       #'`(accessor
           (set . ,fn)
           (enumerable . #t)
           (configurable . #t))]))
  (syntax-case stx ()
    [(_ [name . def] ...)
     (with-syntax
         ([(pname ...) (stx-map parse-name #'(name ...))]
          [(pdesc ...) (stx-map parse-def #'(def ...))])
       #`(let ([obj (new Object%)])
           (define-own-property obj
                 pname
                 pdesc
                 #f) ...
           obj))]))

(define (regexp pattern flags)
  (ecma:new (id RegExp) pattern flags))
