#lang racket/base

(require racket/class
         "../private/error.rkt"
         "../private/function.rkt"
         "../private/object.rkt"
         "../private/types.rkt")

(provide (all-defined-out))

(define-object-properties error-prototype
  ["toString"
   (make-native-function
    (λ (this)
      (unless (is-a? this ecma-object%)
        (error "type error"))
      (define name
        (let ([name (send this get "name")])
          (if (eq? 'undefined name)
              "Error"
              (to-string name))))
      (define msg
        (let ([msg (send this get "message")])
          (if (eq? 'undefined msg)
              ""
              (to-string msg))))
      (cond
        [(string=? "" name) msg]
        [(string=? "" msg) name]
        [else (string-append name ": " msg)])))])
