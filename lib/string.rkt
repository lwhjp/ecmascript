#lang racket/base

(require (only-in racket/class get-field is-a? new)
         racket/list
         racket/string
         "../private/builtin.rkt"
         "../private/error.rkt"
         "../private/function.rkt"
         "../private/object.rkt"
         "../private/primitive.rkt"
         "../private/this.rkt"
         (prefix-in
          ecma:
          (combine-in
           "../convert.rkt")))

(provide get-properties)

(define (get-properties)
  `(["String" . ,string-constructor]))

(define string-constructor
  (letrec
      ([call
        (λ ([value ""])
          (ecma:to-string value))]
       [construct
        (λ ([value ""])
          (new String% [value (ecma:to-string value)]))])
    (make-native-constructor call construct)))

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
     (let ([s (ecma:to-string ecma:this)]
           [p (ecma:to-integer pos)])
       (if (<= 0 p (sub1 (string-length s)))
           (substring s p (add1 p))
           "")))]
  ["charCodeAt"
   (native-method (pos)
     (let ([s (ecma:to-string ecma:this)]
           [p (ecma:to-integer pos)])
       (if (<= 0 p (sub1 (string-length s)))
           (char->integer
            (string-ref s p))
           +nan.0)))]
  ["concat"
   (native-method args
     (apply
      string-append
      (map ecma:to-string (cons ecma:this args))))]
  ["indexOf"
   (native-method (searchString position)
     (let ([s1 (ecma:to-string ecma:this)]
           [s2 (ecma:to-string searchString)]
           [p (ecma:to-integer position)])
       (let ([r (regexp-match-positions s2 s1 p)])
         (if r
             (caar r)
             -1))))]
  ["lastIndexOf"
   (native-method (searchString position)
     (let ([s1 (ecma:to-string ecma:this)]
           [s2 (ecma:to-string searchString)]
           [p (ecma:to-integer position)])
       (let* ([r (regexp-match-positions s2 s1)]
              [r2 (filter (λ (pos)
                            (<= (car pos) p))
                          (or r '()))])
         (if r2
             (car (last r2))
             -1))))]
  ["localeCompare"
   (native-method (that)
     (let ([s (ecma:to-string ecma:this)]
           [that (ecma:to-string that)])
       (cond
         [(string-locale<? s that) -1]
         [(string-locale=? s that) 0]
         [else 1])))]
  ; TODO: match
  ; TODO: replace
  ; TODO: search
  ["slice"
   (native-method (start end)
     (let* ([str (ecma:to-string ecma:this)]
            [len (string-length str)]
            [start (ecma:to-integer start)]
            [end (if (ecma:undefined? end) len (ecma:to-integer end))]
            [from (if (negative? start) (+ len start) start)]
            [to (if (negative? end) (+ len end) end)]
            [span (max (- to from) 0)])
       (substring str from (+ from span))))]
  ; TODO: split
  ["substring"
   (native-method (start end)
     (let* ([s (ecma:to-string ecma:this)]
            [len (string-length s)]
            [int-start (ecma:to-integer start)]
            [int-end (if (ecma:undefined? end) len (ecma:to-integer end))]
            [final-start (min (max int-start 0) len)]
            [final-end (min (max int-end 0) len)]
            [from (min final-start final-end)]
            [to (max final-start final-end)])
       (substring s from to)))]
  ["toLowerCase"
   (native-method ()
     (string-downcase (ecma:to-string ecma:this)))]
  ["toLocaleLowerCase"
   (native-method ()
     (string-downcase (ecma:to-string ecma:this)))]
  ["toUpperCase"
   (native-method ()
     (string-upcase (ecma:to-string ecma:this)))]
  ["toLocaleUpperCase"
   (native-method ()
     (string-upcase (ecma:to-string ecma:this)))]
  ["trim"
   (native-method ()
     (string-trim (ecma:to-string ecma:this)))])
