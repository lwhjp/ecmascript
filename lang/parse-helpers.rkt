#lang racket/base

(require (for-syntax racket/base)
         racket/match
         "../ast.rkt")

(provide (all-defined-out))

(define current-source-name (make-parameter #f))

(define-syntax (location stx)
  (syntax-case stx ()
    ; TODO: get location information from PEG
    [_ (identifier? stx)
       #'(list (current-source-name) #f #f #f #f)]))

(define (merge-locations l r)
  (match-define (list src line column l-pos _) l)
  (match-define (list _ _ _ r-pos r-span) r)
  (define span #f #;(+ (- r-pos l-pos) r-span))
  (list src line column l-pos span))

(define (associate-left xs)
  ; TODO: location
  (if (null? (cdr xs))
      (car xs)
      (foldl/2 (λ (l op r)
                 (define loc
                   (merge-locations (syntax-element-location l)
                                    (syntax-element-location r)))
                 (expression:binary loc l op r))
               (car xs)
               (cdr xs))))

(define (foldl/2 proc init lst)
  (if (null? lst)
      init
      (foldl/2 proc (proc init (car lst) (cadr lst)) (cddr lst))))

(define (fold-chain base chain)
  (foldl (λ (e b) (e b)) base chain))
