#lang typed/racket/base

(require "primitive.rkt"
         "string.rkt"
         "typed-lazy-require.rkt")

(lazy-require/typed
 ["convert.rkt" ([to-string (-> Any ESString)])])

(provide (all-defined-out))

(struct es-exn exn:fail
  ([value : Any])
  #:transparent)

;; TODO: error constructors should be intrinsics
(define native-error-constructor
  : (Parameterof (-> Symbol (U String ESString ESUndefined) Any))
  (make-parameter
   (λ args
     (error 'native-error-constructor "not initialized"))))

(define (raise-native-error [type : Symbol] [message : (U String ESString ESUndefined) ecma:undefined])
  (raise-es-exn ((native-error-constructor) type message)))

(define (raise-es-exn [v : Any])
  : Nothing
  (define message
    (if (string? v)
        v
        (with-handlers ([exn:fail? (λ (e) "(invalid)")])
          (es-string->string
           (to-string v)))))
  (raise (es-exn message (current-continuation-marks) v)))
