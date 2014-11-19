#lang racket/base

(require (except-in racket/class object? this)
         "../object.rkt"
         "../private/builtin.rkt"
         "../private/error.rkt"
         "../private/function.rkt"
         "../private/object.rkt"
         "../private/this.rkt"
         (prefix-in ecma:
                    (combine-in
                     "../private/literal.rkt"
                     "../convert.rkt"
                     "../types.rkt")))

(provide get-properties)

(define (get-properties)
  `(["RegExp" . ,regexp-constructor]))

(define (make-regexp-object pattern flags prototype)
  (let ([obj
         (instantiate regexp% (pattern flags)
           [prototype prototype])])
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
      'null))

(define regexp:prototype
  (make-regexp-object "" "" object:prototype))

(define regexp-constructor
  (letrec
      ([call
        (λ (pattern [flags 'undefined])
          (if (and (is-a? pattern regexp%)
                   (eq? 'undefined flags))
              pattern
              (construct pattern flags)))]
       [construct
        (λ ([pattern ""] [flags 'undefined])
          (define p
            (cond
              [(is-a? pattern regexp%)
               (if (eq? 'undefined flags)
                   (get-field pattern pattern)
                   (raise-native-error 'type))]
              [else (ecma:to-string pattern)]))
          (make-regexp-object
           p
           (if (eq? 'undefined flags) "" (ecma:to-string flags))
           regexp:prototype))])
    (make-native-constructor call construct)))

(define-object-properties regexp-constructor
  ["prototype" regexp:prototype])

(define-object-properties regexp:prototype
  ["constructor" regexp-constructor]
  ["exec"
   (native-method (string)
     (exec-regexp this string))]
  ["test"
   (native-method (string)
     (not (eq? 'null (exec-regexp this string))))]
  ["toString"
   (native-method ()
     (string-append
      "/"
      (ecma:to-string (get-property-value this "source"))
      "/"
      (if (ecma:to-boolean (get-property-value this "global")) "g" "")
      (if (ecma:to-boolean (get-property-value this "ignoreCase")) "i" "")
      (if (ecma:to-boolean (get-property-value this "multiline")) "m" "")))])
