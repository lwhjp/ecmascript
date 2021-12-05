#lang racket/base

(require racket/class
         "../convert.rkt"
         "object.rkt"
         "string.rkt")

(provide ecma-array%)

(define ecma-array%
  (class ecma-object%
    (init [length 0])
    (super-new [class-name 'Array])
    (super define-own-property!
      (string->es-string "length")
      (data-property #f #f length #t))
    (inherit get-own-property delete-property!)
    (define/override (define-own-property! name desc)
      (define old-len-desc (get-own-property (string->es-string "length")))
      (define old-len (data-property-value old-len-desc))
      (cond
        [(es-string=? name (string->es-string "length"))
         ; TODO: delete / writable semantics
         (let ([new-len (cdr (assq 'value (cdr desc)))])
           (for ([i (in-range new-len old-len)])
             (delete-property! (to-string i)))
           (set-data-property-value! old-len-desc new-len))]
        [(string->number (es-string->string name)) ; TODO: validate array index
         => (λ (i)
              ; TODO: writable semantics
              (super define-own-property! name desc)
              (super define-own-property!
                     (string->es-string "length")
                     (data-property #f #f (max (add1 i) old-len) #f))
              #t)]
        [else (super define-own-property! name desc)]))))
