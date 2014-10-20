#lang racket/base

(require racket/class
         racket/string
         "../private/array.rkt"
         "../private/environment.rkt"
         "../private/function.rkt"
         "../private/object.rkt"
         "../private/types.rkt"
         [prefix-in ecma:
                    (combine-in
                     "../private/expression.rkt"
                     "../private/helpers.rkt")])

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
  ["prototype" array-prototype]
  ["isArray"
   (native-method (this arg)
     (is-a? arg array%))])

(define-object-properties array-prototype
  ["constructor" array-constructor]
  ["toString"
   (native-method (this)
     (let* ([array (to-object this)]
            [func (send array get "join")])
       (if (is-a? func function%)
           (send func call this)
           (send (get-value
                  (ecma:member
                   (ecma:member (id Object)
                                "prototype")
                   "toString"))
                 call
                 this))))]
  ["toLocaleString"
   (native-method (this)
     (send (send this get "toString") call this))]
  ; TODO: concat
  ["join"
   (native-method (this separator)
     (string-join
      (for/list ([i (in-range
                     (to-uint32
                      (send this get "length")))])
        (let ([elt (send this get (number->string i))])
          (if (or (eq? 'undefined elt)
                  (eq? 'null elt))
              ""
              (to-string elt))))
      (if (eq? 'undefined separator)
          ","
          (to-string separator))))]
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
