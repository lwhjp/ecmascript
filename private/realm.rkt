#lang racket/base

(require racket/class)

(provide (all-defined-out))

(define realm%
  (class object%
    (init-field global-object
                global-environment)
    (super-new)
    (define/public (clone)
      (make-object realm%
        (send global-object clone)
        (send global-environment clone)))))

(define current-realm (make-parameter #f))

(define (current-global-object)
  (get-field global-object (current-realm)))

(define (current-global-environment)
  (get-field global-environment (current-realm)))

(define (get-global-object) (current-global-object))
