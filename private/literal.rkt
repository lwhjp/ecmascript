#lang racket/base

(require (for-syntax racket/base
                     syntax/parse
                     syntax/stx)
         racket/class
         racket/provide
         "array.rkt"
         "environment.rkt"
         "object.rkt"
         (prefix-in ecma: "../types.rkt"))

(provide (filtered-out
          (λ (name)
            (and (regexp-match? #rx"^ecma:" name)
                 (substring name 5)))
          (all-defined-out)))

(define (ecma:array . elements)
  (let ([obj (new array%
                  [prototype array-prototype])])
    (for ([i (in-naturals)]
          [elt (in-list elements)]
          #:unless (eq? 'undefined elt))
      (send obj define-own-property
            (ecma:to-string i)
            `(data
              (value . ,elt)
              (writable . #t)
              (enumerable . #t)
              (configurable . #t))
            #f))
    (send obj put! "length" (length elements))
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
       #`(let ([obj (new ecma-object%
                         [prototype object-prototype]
                         [class "Object"])])
           (send obj
                 define-own-property
                 pname
                 pdesc
                 #f) ...
           obj))]))

(define (ecma:regexp pattern flags)
  (send (get-value (id RegExp)) construct pattern flags))