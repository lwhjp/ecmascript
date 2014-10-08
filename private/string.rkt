#lang racket/base

(require racket/list
         "object.rkt"
         "function.rkt"
         "types.rkt")

(provide (all-defined-out))

(struct string-object object (value))

(define string-prototype-object
  (string-object
   "String"
   object-prototype-object
   (make-hash
    `(("toString" . ,(native-method (this)
                       (unless (string-object? this)
                         (error "not a string"))
                       (string-object-value this)))
      ("valueOf" . ,(native-method (this)
                      (unless (string-object? this)
                        (error "not a string"))
                      (string-object-value this)))
      ("charAt" . ,(native-method (this pos)
                     (let ([s (to-string this)]
                           [p (to-integer pos)])
                       (if (<= 0 p (sub1 (string-length s)))
                           (substring s p (add1 p))
                           ""))))
      ("charCodeAt" . ,(native-method (this pos)
                         (let ([s (to-string this)]
                               [p (to-integer pos)])
                           (if (<= 0 p (sub1 (string-length s)))
                               (char->integer
                                (string-ref s p))
                               +nan.0))))
      ("indexOf" . ,(native-method (this searchString position)
                      (let ([s1 (to-string this)]
                            [s2 (to-string searchString)]
                            [p (to-integer position)])
                        (let ([r (regexp-match-positions s2 s1 p)])
                          (if r
                              (caar r)
                              -1)))))
      ("lastIndexOf" . ,(native-method (this searchString position)
                          (let ([s1 (to-string this)]
                                [s2 (to-string searchString)]
                                [p (to-integer position)])
                            (let* ([r (regexp-match-positions s2 s1)]
                                   [r2 (filter (λ (pos)
                                                 (<= (car pos) p))
                                               (or r '()))])
                              (if r2
                                  (car (last r2))
                                  -1)))))
      ; TODO: split
      ; TODO: substring
      ("toLowerCase" . ,(native-method (this)
                          (string-downcase (to-string this))))
      ("toUpperCase" . ,(native-method (this)
                          (string-upcase (to-string this))))))
   ""))

(define (make-string-object v)
  (unless (string? v)
    (raise-argument-error 'make-string-object "string?" v))
  (string-object
   (object-class string-prototype-object)
   string-prototype-object
   (make-hash
    `(("length" . ,(make-property (string-length v)))))
   v))
