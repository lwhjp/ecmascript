#lang racket/base

(require (only-in racket/class get-field)
         racket/string
         "../object.rkt"
         "../private/array.rkt"
         "../private/environment.rkt"
         "../private/function.rkt"
         "../private/object.rkt"
         "../private/primitive.rkt"
         "../private/this.rkt"
         [prefix-in ecma:
                    (combine-in
                     "../private/literal.rkt"
                     "../convert.rkt")])

(provide get-properties)

(define (get-properties)
  `(["Array" . ,array-constructor]))

(define array-constructor
  (letrec
      ([call
        (λ args
          (apply construct args))]
       [construct
        (λ args
          (apply ecma:array args))])
    (make-native-constructor call construct)))

(define-object-properties array-constructor
  ["prototype" Array:prototype]
  ["isArray"
   (native-method (arg)
     (Array? arg))])

(define-object-properties Array:prototype
  ["constructor" array-constructor]
  ["toString"
   (native-method ()
     (let* ([array (ecma:to-object ecma:this)]
            [func (get-property-value array "join")])
       (define proc
         (if (Function? func)
             (get-field proc func)
             (get-field proc (get-value
                              (member
                               (member (id Object)
                                       "prototype")
                               "toString")))))
       (proc)))]
  ["toLocaleString"
   (native-method ()
     ((get-field proc (get-property-value ecma:this "toString"))))]
  ; TODO: concat
  ["join"
   (native-method (separator)
     (string-join
      (for/list ([i (in-range
                     (ecma:to-uint32
                      (get-property-value ecma:this "length")))])
        (let ([elt (get-property-value ecma:this (number->string i))])
          (if (or (ecma:undefined? elt)
                  (ecma:null? elt))
              ""
              (ecma:to-string elt))))
      (if (ecma:undefined? separator)
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
