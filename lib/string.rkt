#lang racket/base

(require racket/class
         racket/lazy-require
         racket/list
         racket/string
         "../private/error.rkt"
         "../private/object.rkt"
         "../private/primitive.rkt"
         "../private/this.rkt"
         (only-in "function.rkt" Function%)
         (only-in "object.rkt" Object:prototype)
         "util.rkt")

(lazy-require
 ["../convert.rkt" (to-integer to-string)])

(provide get-properties
 make-String)

(define (get-properties)
  `(["String" . ,string-constructor]))

(define String%
  (class ecma-object%
    (init [prototype String:prototype])
    (init-field [value ""])
    (super-new [class-name 'String]
               [prototype prototype])))

(define String:prototype
  (new String% [prototype Object:prototype]))

(define (make-String value)
  (new String% [value value]))

(define string-constructor
  (new
   (class Function%
     (super-new [formal-parameters '(value)]
                [proc to-string])
     (define/override (construct args)
       (let ([value (if (null? args) ecma:undefined (car args))])
         (make-String (to-string value)))))))

(define-object-properties string-constructor
  ["prototype" String:prototype]
  ["fromCharCode"
   (native-method args
     (list->string
      (map integer->char args)))])

(define-object-properties String:prototype
  ["constructor" string-constructor]
  ["toString"
   (native-method ()
     (unless (is-a? ecma:this String%)
       (raise-native-error 'type "not a string"))
     (get-field value ecma:this))]
  ["valueOf"
   (native-method ()
     (unless (is-a? ecma:this String%)
       (raise-native-error 'type "not a string"))
     (get-field value ecma:this))]
  ["charAt"
   (native-method (pos)
     (let ([s (to-string ecma:this)]
           [p (to-integer pos)])
       (if (<= 0 p (sub1 (string-length s)))
           (substring s p (add1 p))
           "")))]
  ["charCodeAt"
   (native-method (pos)
     (let ([s (to-string ecma:this)]
           [p (to-integer pos)])
       (if (<= 0 p (sub1 (string-length s)))
           (char->integer
            (string-ref s p))
           +nan.0)))]
  ["concat"
   (native-method args
     (apply
      string-append
      (map to-string (cons ecma:this args))))]
  ["indexOf"
   (native-method (searchString position)
     (let ([s1 (to-string ecma:this)]
           [s2 (to-string searchString)]
           [p (to-integer position)])
       (let ([r (regexp-match-positions s2 s1 p)])
         (if r
             (caar r)
             -1))))]
  ["lastIndexOf"
   (native-method (searchString position)
     (let ([s1 (to-string ecma:this)]
           [s2 (to-string searchString)]
           [p (to-integer position)])
       (let* ([r (regexp-match-positions s2 s1)]
              [r2 (filter (λ (pos)
                            (<= (car pos) p))
                          (or r '()))])
         (if r2
             (car (last r2))
             -1))))]
  ["localeCompare"
   (native-method (that)
     (let ([s (to-string ecma:this)]
           [that (to-string that)])
       (cond
         [(string-locale<? s that) -1]
         [(string-locale=? s that) 0]
         [else 1])))]
  ; TODO: match
  ; TODO: replace
  ; TODO: search
  ["slice"
   (native-method (start end)
     (let* ([str (to-string ecma:this)]
            [len (string-length str)]
            [start (to-integer start)]
            [end (if (ecma:undefined? end) len (to-integer end))]
            [from (if (negative? start) (+ len start) start)]
            [to (if (negative? end) (+ len end) end)]
            [span (max (- to from) 0)])
       (substring str from (+ from span))))]
  ; TODO: split
  ["substring"
   (native-method (start end)
     (let* ([s (to-string ecma:this)]
            [len (string-length s)]
            [int-start (to-integer start)]
            [int-end (if (ecma:undefined? end) len (to-integer end))]
            [final-start (min (max int-start 0) len)]
            [final-end (min (max int-end 0) len)]
            [from (min final-start final-end)]
            [to (max final-start final-end)])
       (substring s from to)))]
  ["toLowerCase"
   (native-method ()
     (string-downcase (to-string ecma:this)))]
  ["toLocaleLowerCase"
   (native-method ()
     (string-downcase (to-string ecma:this)))]
  ["toUpperCase"
   (native-method ()
     (string-upcase (to-string ecma:this)))]
  ["toLocaleUpperCase"
   (native-method ()
     (string-upcase (to-string ecma:this)))]
  ["trim"
   (native-method ()
     (string-trim (to-string ecma:this)))])
