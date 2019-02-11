#lang racket/base

(require racket/class
         racket/lazy-require
         "../private/error.rkt"
         "../private/object.rkt"
         "../private/primitive.rkt"
         "../private/this.rkt"
         (only-in "array.rkt" make-array)
         (only-in "function.rkt" Function%)
         (only-in "object.rkt" Object:prototype)
         "util.rkt")

(lazy-require
 ["../convert.rkt" (to-boolean to-string)])

(provide get-properties)

(define (get-properties)
  `(["RegExp" . ,regexp-constructor]))

(define RegExp%
  (class ecma-object%
    (init prototype)
    (init-field pattern flags)
    (super-new [class-name 'RegExp]
               [prototype prototype])))

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
                make-array
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
  (new
   (class Function%
     (super-new [formal-parameters '(pattern flags)]
                [proc (λ (pattern [flags ecma:undefined])
                        (if (and (is-a? pattern RegExp%)
                                 (ecma:undefined? flags))
                            pattern
                            (construct . (list pattern flags))))])
     (define/override (construct args)
       (apply
        (λ ([pattern ""] [flags ecma:undefined])
          (define p
            (cond
              [(is-a? pattern RegExp%)
               (if (ecma:undefined? flags)
                   (get-field pattern pattern)
                   (raise-native-error 'type))]
              [else (to-string pattern)]))
          (make-regexp-object
           p
           (if (ecma:undefined? flags) "" (to-string flags))
           regexp:prototype))
        args)))))

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
      (to-string (get-property-value ecma:this "source"))
      "/"
      (if (to-boolean (get-property-value ecma:this "global")) "g" "")
      (if (to-boolean (get-property-value ecma:this "ignoreCase")) "i" "")
      (if (to-boolean (get-property-value ecma:this "multiline")) "m" "")))])
