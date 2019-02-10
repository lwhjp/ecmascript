#lang racket/base

(require racket/class
         "object.rkt")

(provide (all-defined-out))

(define Array%
  (class ecma-object%
    (init [prototype Array:prototype]
          [length 0])
    (super-new [class-name 'Array]
               [prototype prototype])
    (super define-own-property
      "length"
      `(data (value . ,length)
             (writable . #t)
             (enumerable . #f)
             (configurable . #f))
      #f)
    (inherit get-own-property delete)
    (define/override (define-own-property name desc throw?)
      (define old-len-desc (get-own-property "length"))
      (define old-len (data-property-value old-len-desc))
      (cond
        [(string=? name "length")
         ; TODO: delete / writable semantics
         (let ([new-len (cdr (assq 'value (cdr desc)))])
           (for ([i (in-range new-len old-len)])
             (delete (number->string i)))
           (set-data-property-value! old-len-desc new-len))]
        [(string->number name) ; TODO: validate array index
         => (λ (i)
              ; TODO: writable semantics
              (super define-own-property name desc throw?)
              (super define-own-property "length" `(data (value . ,(max (add1 i) old-len))) #f)
              #t)]
        [else (super define-own-property name desc throw?)]))))

(define Array:prototype
  (new Array% [prototype Object:prototype]))

(define (Array? v) (is-a? v Array%))
