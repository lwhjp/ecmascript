#lang racket/base

(require (only-in racket/class get-field is-a? new)
         "../object.rkt"
         "../private/builtin.rkt"
         "../private/error.rkt"
         "../private/function.rkt"
         "../private/object.rkt"
         "../private/primitive.rkt"
         "../private/this.rkt"
         (prefix-in ecma:
                    (combine-in
                     "../private/literal.rkt"
                     "../convert.rkt")))

(provide get-properties)

(define (get-properties)
  `(["RegExp" . ,regexp-constructor]))

(define (make-regexp-object pattern flags prototype)
  (let ([obj (new RegExp%
                  [prototype prototype]
                  [pattern pattern]
                  [flags flags])])
    (define-object-properties obj
      ["source" pattern]
      ["global" (regexp-match? "g" flags)]
      ["ignoreCase" (regexp-match? "i" flags)]
      ["multiline" (regexp-match? "m" flags)]
      ["lastIndex" 0])
    obj))

(module+ test
  (define r (make-regexp-object "foo" "gi" regexp:prototype))
  (exec-regexp r "fOo foo")
  (exec-regexp r "fOo foo"))

(define (exec-regexp r s)
  (define-values (global ignore-case multiline last-index)
    (apply values
           (map (λ (p)
                  (get-property-value r p))
                '("global" "ignoreCase" "multiline" "lastIndex"))))
  (define re
    (pregexp
     (format
      "(?~a:~a)"
      (string-append
       (if ignore-case "i" "")
       (if multiline "m" ""))
      (get-field pattern r))))
  (define positions
    (regexp-match-positions
     re
     s
     (if global last-index 0)))
  (if positions
      (let ([a (apply
                ecma:array
                (map (λ (p)
                       (substring s (car p) (cdr p)))
                     positions))])
        (when global
          (set-property-value! r "lastIndex" (cdar positions)))
        (define-object-properties a
          ["index" (caar positions)]
          ["input" s])
        a)
      ecma:null))

(define regexp:prototype
  (make-regexp-object "" "" Object:prototype))

(define regexp-constructor
  (letrec
      ([call
        (λ (pattern [flags ecma:undefined])
          (if (and (is-a? pattern RegExp%)
                   (ecma:undefined? flags))
              pattern
              (construct pattern flags)))]
       [construct
        (λ ([pattern ""] [flags ecma:undefined])
          (define p
            (cond
              [(is-a? pattern RegExp%)
               (if (ecma:undefined? flags)
                   (get-field pattern pattern)
                   (raise-native-error 'type))]
              [else (ecma:to-string pattern)]))
          (make-regexp-object
           p
           (if (ecma:undefined? flags) "" (ecma:to-string flags))
           regexp:prototype))])
    (make-native-constructor call construct)))

(define-object-properties regexp-constructor
  ["prototype" regexp:prototype])

(define-object-properties regexp:prototype
  ["constructor" regexp-constructor]
  ["exec"
   (native-method (string)
     (exec-regexp ecma:this string))]
  ["test"
   (native-method (string)
     (not (ecma:null? (exec-regexp ecma:this string))))]
  ["toString"
   (native-method ()
     (string-append
      "/"
      (ecma:to-string (get-property-value ecma:this "source"))
      "/"
      (if (ecma:to-boolean (get-property-value ecma:this "global")) "g" "")
      (if (ecma:to-boolean (get-property-value ecma:this "ignoreCase")) "i" "")
      (if (ecma:to-boolean (get-property-value ecma:this "multiline")) "m" "")))])
