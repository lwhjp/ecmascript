#lang racket/base

(require (only-in racket/class get-field)
         racket/match
         racket/math
         racket/runtime-path
         net/uri-codec
         "private/function.rkt"
         "private/global-object.rkt"
         "private/object.rkt"
         "private/this.rkt"
         "convert.rkt"
         "types.rkt"
         (prefix-in ecma: "eval.rkt"))

(this-binding global-object)

(define (import-library mod)
  (define imported-properties
    ((dynamic-require mod 'get-properties)))
  (define global-property-map
    (get-field properties global-object))
  (for ([prop (in-list imported-properties)])
    (match-define (cons name value) prop)
    (hash-set! global-property-map
               name
               (if (property? value)
                   value
                   (make-data-property
                    value
                    #:writable #t
                    #:enumerable #f
                    #:configurable #f)))))

(define-object-properties global-object
  ["NaN" +nan.0]
  ["Infinity" +inf.0]
  ["eval"
   (native-method (x)
     (if (string? x)
         (ecma:eval x)
         x))]
  ["parseInt"
   (native-method (string radix)
     (string->number (to-string string)
                     (to-int32 radix)))]
  ["parseFloat"
   (native-method (string)
     (string->number (to-string string)))]
  ["isNaN"
   (native-method (number)
     (nan? (to-number number)))]
  ["isFinite"
   (native-method (number)
     (not (infinite? number)))]
  ["decodeURI"
   (native-method (encodedURI)
     (uri-decode (to-string encodedURI)))]
  ["decodeURIComponent"
   (native-method (encodedURIComponent)
     (uri-path-segment-decode (to-string encodedURIComponent)))]
  ["encodeURI"
   (native-method (uri)
     (uri-encode (to-string uri)))]
  ["encodeURIComponent"
   (native-method (uriComponent)
     (uri-path-segment-encode (to-string uriComponent)))])

(define-runtime-module-path lib:array "lib/array.rkt")
(define-runtime-module-path lib:boolean "lib/boolean.rkt")
(define-runtime-module-path lib:error "lib/error.rkt")
(define-runtime-module-path lib:function "lib/function.rkt")
(define-runtime-module-path lib:math "lib/math.rkt")
(define-runtime-module-path lib:number "lib/number.rkt")
(define-runtime-module-path lib:object "lib/object.rkt")
(define-runtime-module-path lib:regexp "lib/regexp.rkt")
(define-runtime-module-path lib:string "lib/string.rkt")

(for-each import-library
          (list lib:array
                lib:boolean
                lib:error
                lib:function
                lib:math
                lib:number
                lib:object
                lib:regexp
                lib:string))
