#lang racket/base

(require (for-syntax racket/base)
         racket/class
         racket/math
         racket/provide
         "../private/environment.rkt"
         "../private/error.rkt"
         "../private/function.rkt"
         "../private/object.rkt"
         "../private/primitive.rkt"
         "../convert.rkt")

(provide (filtered-out
          (λ (name)
            (substring name 3))
          (all-defined-out)))

(define (op:post++ v)
  (define r (to-number (get-value v)))
  (put-value! v (op:+ r 1))
  r)

(define (op:post-- v)
  (define r (to-number (get-value v)))
  (put-value! v (op:- r 1))
  r)

(define (op:delete ref)
  (cond
    [(not (reference? ref)) #t]
    [(ecma:undefined? (reference-base ref))
     (if (reference-strict? ref)
         (raise-native-error 'syntax)
         #t)]
    [(is-a? (reference-base ref) environment-record%)
     (if (reference-strict? ref)
         (raise-native-error 'syntax)
         (send (reference-base ref)
               delete-binding!
               (reference-name ref)))]
    [else
     (delete-property!
      (to-object (reference-base ref))
      (reference-name ref)
      (reference-strict? ref))]))

(define (op:void v)
  (get-value v)
  ecma:undefined)

(define (op:typeof v)
  (if (and (reference? v)
           (ecma:undefined? (reference-base v)))
      "undefined"
      (let ([v (get-value v)])
        (cond
          [(ecma:undefined? v) "undefined"]
          [(ecma:null? v) "object"]
          [(boolean? v) "boolean"]
          [(number? v) "number"]
          [(string? v) "string"]
          [(Function? v) "function"]
          [(Object? v) "object"]))))

(define (op:instanceof a b)
  (define lval (get-value a))
  (define rval (get-value b))
  (unless (Function? rval)
    (raise-native-error 'type "not a function"))
  (has-instance? rval lval))

(define (op:++ v)
  (define r
    (op:+ (to-number (get-value v)) 1))
  (put-value! v r)
  r)

(define (op:-- v)
  (define r
    (op:- (to-number (get-value v)) 1))
  (put-value! v r)
  r)

(define (op:~ v)
  (to-int32
   (bitwise-not
    (to-int32
     (get-value v)))))

(define (op:! v)
  (not (to-boolean (get-value v))))

(define op:+
  (case-lambda
    [(x) (to-number (get-value x))]
    [(a b)
     (let ([l (to-primitive (get-value a))]
           [r (to-primitive (get-value b))])
       (if (or (string? l) (string? r))
           (string-append (to-string l) (to-string r))
           (+ (to-number l) (to-number r))))]))

(define-values (op:- op:* op:/ op:%)
  (apply
   values
   (map (λ (op)
          (λ args
            (apply
             op
             (map (λ (v)
                    (to-number
                     (get-value v)))
                  args))))
        (list - * / modulo))))

(define-values (op:<< op:>> op:>>>)
  (apply
   values
   (map (λ (op)
          (λ (n m)
            (op (to-int32 (get-value n))
                (bitwise-and
                 #x1F
                 (to-uint32 (get-value m))))))
        (list (λ (n m)
                (arithmetic-shift n m))
              (λ (n m)
                (arithmetic-shift n (- m)))
              (λ (n m)
                (arithmetic-shift n (- m)))))))

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
     (λ (a b)
       (compare (get-value a) (get-value b) #f))
     (λ (a b)
       (compare (get-value b) (get-value a) #f))
     (λ (a b)
       (not
        (compare (get-value b) (get-value a) #t)))
     (λ (a b)
       (not
        (compare (get-value a) (get-value b) #t))))))

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
     (λ (a b)
       (compare (get-value a)
                (get-value b)))
     (λ (a b)
       (not (compare (get-value a)
                     (get-value b)))))))

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
     (λ (a b)
       (compare (get-value a)
                (get-value b)))
     (λ (a b)
       (not (compare (get-value a)
                     (get-value b)))))))

(define-values (op:& op:^ op:\|)
  (apply
   values
   (map (λ (op)
          (λ (a b)
            (op (to-int32 (get-value a))
                (to-int32 (get-value b)))))
        (list bitwise-and bitwise-xor bitwise-ior))))

(define-syntax-rule (op:&& a b)
  (let ([lval (get-value a)])
    (if (to-boolean lval)
        (get-value b)
        lval)))

(define-syntax-rule (op:\|\| a b)
  (let ([lval (get-value a)])
    (if (to-boolean lval)
        lval
        (get-value b))))

(define-syntax-rule (op:?: test true false)
  (if (to-boolean (get-value test))
      (get-value true)
      (get-value false)))

(define (op:= a b)
  (define v (get-value b))
  (put-value! a v)
  v)

(define-values (op:+= op:-= op:*= op:/= op:%= op:<<= op:>>=
                op:>>>= op:&= op:^= op:\|=)
  (apply
   values
   (map (λ (op)
          (λ (a b)
            (define v
              (op (get-value a)
                  (get-value b)))
            (put-value! a v)
            v))
        (list op:+ op:- op:* op:/ op:% op:<< op:>> op:>>> op:& op:^ op:\|))))

(define (op:\, left right)
  (begin
    (get-value left)
    (get-value right)))
