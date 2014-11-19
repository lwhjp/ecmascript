#lang racket/base

(require (only-in racket/class is-a? send)
         racket/string
         "../object.rkt"
         "../private/array.rkt"
         "../private/environment.rkt"
         "../private/function.rkt"
         "../private/object.rkt"
         [prefix-in ecma:
                    (combine-in
                     "../private/literal.rkt"
                     "../convert.rkt"
                     "../types.rkt")])

(provide get-properties)

(define (get-properties)
  `(["Array" . ,array-constructor]))

(define array-constructor
  (letrec
      ([call
        (λ (this . args)
          (apply construct args))]
       [construct
        (λ args
          (apply ecma:array args))])
    (make-native-constructor call construct)))

(define-object-properties array-constructor
  ["prototype" array:prototype]
  ["isArray"
   (native-method (this arg)
     (is-a? arg array%))])

(define-object-properties array:prototype
  ["constructor" array-constructor]
  ["toString"
   (native-method (this)
     (let* ([array (ecma:to-object this)]
            [func (get-property-value array "join")])
       (if (is-a? func function%)
           (send func call this)
           (send (get-value
                  (member
                   (member (id Object)
                           "prototype")
                   "toString"))
                 call
                 this))))]
  ["toLocaleString"
   (native-method (this)
     (send (get-property-value this "toString") call this))]
  ; TODO: concat
  ["join"
   (native-method (this separator)
     (string-join
      (for/list ([i (in-range
                     (ecma:to-uint32
                      (get-property-value this "length")))])
        (let ([elt (get-property-value this (number->string i))])
          (if (or (ecma:undefined? elt)
                  (ecma:null? elt))
              ""
              (ecma:to-string elt))))
      (if (eq? 'undefined separator)
          ","
          (ecma:to-string separator))))]
  ; TODO: pop
  ; TODO: push
  ; TODO: reverse
  ; TODO: shift
  ; TODO: slice
  ; TODO: sort
  ; TODO: splice
  ; TODO: unshift
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
