#lang racket/base

(require racket/class
         racket/match
         racket/runtime-path
         "private/global-object.rkt"
         "private/object.rkt")

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
  ; TODO: eval
  ; TODO: parseInt
  ; TODO: parseFloat
  ; TODO: escape
  ; TODO: unescape
  ; TODO: isNaN
  ; TODO: isFinite
  )

(define-runtime-module-path lib:boolean "lib/boolean.rkt")
(define-runtime-module-path lib:error "lib/error.rkt")
(define-runtime-module-path lib:function "lib/function.rkt")
(define-runtime-module-path lib:math "lib/math.rkt")
(define-runtime-module-path lib:number "lib/number.rkt")
(define-runtime-module-path lib:object "lib/object.rkt")
(define-runtime-module-path lib:regexp "lib/regexp.rkt")
(define-runtime-module-path lib:string "lib/string.rkt")

(for-each import-library
          (list lib:boolean
                lib:error
                lib:function
                lib:math
                lib:number
                lib:object
                lib:regexp
                lib:string))
