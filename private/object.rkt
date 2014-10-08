#lang racket/base

(require (for-syntax racket/base))

(provide (all-defined-out))

(struct property ([value #:mutable]
                  read-only?
                  dont-enum
                  dont-delete))

(define (make-property value)
  (property value #f #f #f))

(struct object (class prototype properties))

(define object-prototype-object
  (object "Object" 'null (make-hash)))

(define (get o p)
  (let ([prop (hash-ref (object-properties o) p #f)])
    (if prop
        (property-value prop)
        (let ([proto (object-prototype o)])
          (if (eq? 'null proto)
              'undefined
              (get proto p))))))

(define (put! o p v)
  (define prop
    (hash-ref (object-properties o) p #f))
  (if prop
      (unless (property-read-only? prop)
        (set-property-value! prop v))
      (hash-set! (object-properties o)
                 p
                 (property v #f #f #f))))

(define (has-property? o p)
  (let ([prop (hash-ref (object-properties o) p #f)])
    (if prop
        #t
        (let ([proto (object-prototype o)])
          (and (not (eq? 'null proto))
               (has-property? proto p))))))

(define (delete! o p)
  (define prop
    (hash-ref (object-properties o) p #f))
  (or (not prop)
      (and (not (property-dont-delete prop))
           (begin
             (hash-remove! (object-properties o) p)
             #t))))
