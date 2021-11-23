#lang racket/base

(require (for-syntax racket/base)
         racket/math
         racket/provide
         "../private/error.rkt"
         "../private/function.rkt"
         "../private/object.rkt"
         "../private/primitive.rkt"
         "../convert.rkt"
         "reference.rkt")

(provide (filtered-out
          (λ (name)
            (substring name 3))
          (all-defined-out)))

(define-syntax-rule (op:post++ v)
  (post-update-reference! v (λ (x) (op:+ x 1))))

(define-syntax-rule (op:post-- v)
  (post-update-reference! v (λ (x) (op:- x 1))))

(define (op:void v)
  ecma:undefined)

(define (op:instanceof a b)
  (unless (Function? b)
    (raise-native-error 'type "not a function"))
  (has-instance? b a))

(define-syntax-rule (op:++ v)
  (update-reference! v (λ (x) (op:+ x 1))))

(define-syntax-rule (op:-- v)
  (update-reference! v (λ (x) (op:- x 1))))

(define (op:~ v)
  (to-int32
   (bitwise-not
    (to-int32 v))))

(define (op:! v)
  (not (to-boolean v)))

(define op:+
  (case-lambda
    [(x) (to-number x)]
    [(a b)
     (let ([l (to-primitive a)]
           [r (to-primitive b)])
       (if (or (string? l) (string? r))
           (string-append (to-string l) (to-string r))
           (+ (to-number l) (to-number r))))]))

(define-values (op:- op:* op:/ op:%)
  (apply
   values
   (map (λ (op)
          (λ (a b)
            (op (to-number a) (to-number b))))
        (list - * / modulo))))

(define-values (op:<< op:>> op:>>>)
  (apply
   values
   (map (λ (op)
          (λ (n m)
            (op (to-int32 n)
                (bitwise-and #x1F (to-uint32 m)))))
        (list (λ (n m) (arithmetic-shift n m))
              (λ (n m) (arithmetic-shift n (- m)))
              (λ (n m) (arithmetic-shift n (- m)))))))

(define-values (op:< op:> op:<= op:>=)
  (let ([compare
         (λ (x y undef)
           (let ([xp (to-primitive x)]
                 [yp (to-primitive y)])
             (if (and (string? xp) (string? yp))
                 (string<? xp yp)
                 (let ([xn (to-number x)]
                       [yn (to-number y)])
                   (cond
                     [(nan? xn) undef]
                     [(nan? yn) undef]
                     [else (< xn yn)])))))])
    (values
     (λ (a b) (compare a b #f))
     (λ (a b) (compare b a #f))
     (λ (a b) (not (compare b a #t)))
     (λ (a b) (not (compare a b #t))))))

(define-values (op:== op:!=)
  (let ([compare
         (λ (a b)
           (cond
             [(ecma:undefined? a)
              (or (ecma:undefined? b)
                  (ecma:null? b))]
             [(ecma:null? a)
              (or (ecma:null? b)
                  (ecma:undefined? b))]
             [(boolean? a)
              (if (boolean? b)
                  (eq? a b)
                  (op:== (to-number a) b))]
             [(boolean? b) (op:== a (to-number b))]
             [(number? a)
              (cond
                [(number? b) (= a b)]
                [(string? b) (op:== a (to-number b))]
                [(Object? b) (op:== a (to-primitive b))]
                [else #f])]
             [(string? a)
              (cond
                [(string? b) (string=? a b)]
                [(number? b) (op:== (to-number a) b)]
                [(Object? b) (op:== a (to-primitive b))]
                [else #f])]
             [(Object? a)
              (cond
                [(Object? b) (eq? a b)]
                [(or (string? b) (number? b)) (op:== (to-primitive a) b)]
                [else #f])]))])
    (values
     (λ (a b) (compare a b))
     (λ (a b) (not (compare a b))))))

(define-values (op:=== op:!==)
  (let ([compare
         (λ (a b)
           (cond
             [(number? a)
              (and (number? b)
                   (= a b))]
             [(string? a)
              (and (string? b)
                   (string=? a b))]
             [else (eq? a b)]))])
    (values
     (λ (a b) (compare a b))
     (λ (a b) (not (compare a b))))))

(define-values (op:& op:^ op:\|)
  (apply
   values
   (map (λ (op)
          (λ (a b)
            (op (to-int32 a) (to-int32 b))))
        (list bitwise-and bitwise-xor bitwise-ior))))

(define-syntax-rule (op:&& a b)
  (let ([lval a])
    (if (to-boolean lval)
        b
        lval)))

(define-syntax-rule (op:\|\| a b)
  (let ([lval a])
    (if (to-boolean lval)
        lval
        b)))

(define-syntax-rule (op:?: test true false)
  (if (to-boolean test)
      true
      false))

(define-syntax-rule (op:= a b)
  (let ([v b])
    (set-reference! a v)
    v))

(define-syntaxes (op:+= op:-= op:*= op:/= op:%= op:<<= op:>>=
                  op:>>>= op:&= op:^= op:\|=)
  (apply
   values
   (map (λ (op)
          (λ (stx)
            (syntax-case stx ()
              [(_ a b)
               (with-syntax ([op op])
                 #'(update-reference! a (λ (x) (op x b))))])))
        (list #'op:+ #'op:- #'op:* #'op:/ #'op:% #'op:<< #'op:>> #'op:>>> #'op:& #'op:^ #'op:\|))))

(define-syntax-rule (op:\, left right)
  (begin left right))
