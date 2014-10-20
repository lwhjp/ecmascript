#lang racket/base

(require racket/class
         "../private/error.rkt"
         "../private/function.rkt"
         "../private/object.rkt"
         "../private/types.rkt"
         [prefix-in ecma: "../private/helpers.rkt"])

(provide get-properties)

(define (get-properties)
  `(["RegExp" . ,regexp-constructor]))

(define regexp%
  (class ecma-object%
    (init-field pattern flags)
    (super-new [class "RegExp"])))

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
  (define r (make-regexp-object "foo" "gi" regexp-prototype))
  (exec-regexp r "fOo foo")
  (exec-regexp r "fOo foo"))

(define (exec-regexp r s)
  (define-values (global ignore-case multiline last-index)
    (apply values
           (map (λ (p)
                  (send r get p))
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
          (send r put! "lastIndex" (cdar positions)))
        (define-object-properties a
          ["index" (caar positions)]
          ["input" s])
        a)
      'null))

(define regexp-prototype
  (make-regexp-object "" "" object-prototype))

(define regexp-constructor
  (letrec
      ([call
        (λ (this pattern [flags 'undefined])
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
              [else (to-string pattern)]))
          (make-regexp-object
           p
           (if (eq? 'undefined flags) "" (to-string flags))
           regexp-prototype))])
    (make-native-constructor call construct)))

(define-object-properties regexp-constructor
  ["prototype" regexp-prototype])

(define-object-properties regexp-prototype
  ["constructor" regexp-constructor]
  ["exec"
   (native-method (this string)
     (exec-regexp this string))]
  ["test"
   (native-method (this string)
     (not (eq? 'null (exec-regexp this string))))]
  ["toString"
   (native-method (this)
     (string-append
      "/"
      (to-string (send this get "source"))
      "/"
      (if (to-boolean (send this get "global")) "g" "")
      (if (to-boolean (send this get "ignoreCase")) "i" "")
      (if (to-boolean (send this get "multiline")) "m" "")))])
