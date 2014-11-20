#lang racket/base

(require (for-syntax racket/base
                     syntax/parse
                     syntax/stx)
         racket/provide
         "../object.rkt"
         "array.rkt"
         "environment.rkt"
         "object.rkt"
         (prefix-in
          ecma:
          (combine-in
           "../convert.rkt"
           "../function.rkt"
           "../types.rkt")))

(provide (filtered-out
          (λ (name)
            (and (regexp-match? #rx"^ecma:" name)
                 (substring name 5)))
          (all-defined-out)))

(define (ecma:array . elements)
  (let ([obj (Array Array:prototype (make-hash) #t)])
    (for ([i (in-naturals)]
          [elt (in-list elements)]
          #:unless (eq? 'undefined elt))
      (define-own-property obj
            (ecma:to-string i)
            `(data
              (value . ,elt)
              (writable . #t)
              (enumerable . #t)
              (configurable . #t))
            #f))
    (set-property-value! obj "length" (length elements))
    obj))

(define-syntax (ecma:object stx)
  (define parse-name
    (syntax-parser
      [v:id (symbol->string (syntax-e #'v))]
      [v:str #'v]
      [v:number #'(ecma:to-string v)]))
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
       #`(let ([obj (Object Object:prototype (make-hash) #t)])
           (define-own-property obj
                 pname
                 pdesc
                 #f) ...
           obj))]))

(define (ecma:regexp pattern flags)
  (ecma:new (id RegExp) pattern flags))
