#lang racket/base

(require racket/class
         racket/lazy-require
         racket/string
         "../private/array.rkt"
         "../private/environment.rkt"
         "../private/function.rkt"
         "../private/object.rkt"
         "../private/primitive.rkt"
         "../private/string.rkt"
         "../private/this.rkt"
         (only-in "object.rkt" Object:prototype)
         "util.rkt")

(lazy-require
 ["../convert.rkt" (to-integer to-object to-string to-uint32)])

(provide get-properties
 Array%
 make-array)

(define (get-properties)
  `(["Array" . ,array-constructor]))

(define Array%
  (class ecma-array%
    (init [prototype Array:prototype])
    (super-new [prototype prototype])))

(define Array:prototype
  (new Array% [prototype Object:prototype]))

(define (make-array . elements)
  (let ([obj (new Array%)])
    (for ([i (in-naturals)]
          [elt (in-list elements)]
          #:unless (ecma:undefined? elt))
      (define-own-property obj
            (es-string->string (to-string i))
            `(data
              (value . ,elt)
              (writable . #t)
              (enumerable . #t)
              (configurable . #t))
            #f))
    obj))

(define array-constructor
  (make-native-function make-array))

(define-object-properties array-constructor
  ["prototype" Array:prototype]
  ["isArray"
   (native-method (arg)
     (is-a? arg ecma-array%))])

(define-object-properties Array:prototype
  ["constructor" array-constructor]
  ["toString"
   (native-method ()
     (let* ([array (to-object ecma:this)]
            [func (get-property-value array "join")]
            [func (if (Function? func) func (get-value Object:prototype "toString"))])
       (send func call ecma:this '())))]
  ["toLocaleString"
   (native-method ()
     (send (get-property-value ecma:this "toString") call ecma:this '()))]
  ; TODO: concat
  ["join"
   (native-method (separator)
     (string-join
      (for/list ([i (in-range
                     (to-uint32
                      (get-property-value ecma:this "length")))])
        (let ([elt (get-property-value ecma:this (number->string i))])
          (if (or (ecma:undefined? elt)
                  (ecma:null? elt))
              ""
              (to-string elt))))
      (if (ecma:undefined? separator)
          ","
          (to-string separator))))]
  ; TODO: reverse
  ["slice"
   (native-method (start end)
     (define o (to-object ecma:this))
     (define a (new Array%))
     (define len (to-uint32 (send o get "length")))
     (define relative-start (to-integer start))
     (define k
       (if (negative? relative-start)
           (max (+ len relative-end) 0)
           (min relative-start len)))
     (define relative-end (if (ecma:undefined? end) len (to-integer end)))
     (define final
       (if (negative? relative-end)
           (max (+ len relative-end) 0)
           (min relative-end len)))
     (for ([n (in-naturals)]
           [k (in-range k final)])
       (define Pk (to-string k))
       (when (send o has-property? Pk)
         (send a define-own-property
               (to-string n)
               `(data
                 (value . ,(send o get Pk))
                 (writable . #t)
                 (enumerable . #t)
                 (configurable . #t))
               #f)))
     a)]
  ; TODO: sort
  ; TODO: splice
  ; TODO: indexOf
  ; TODO: lastIndexOf
  ; TODO: every
  ; TODO: some
  ; TODO: forEach
  ; TODO: map
  ; TODO: filter
  ; TODO: reduce
  ; TODO: reduceRight
  )
