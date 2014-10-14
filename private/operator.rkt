#lang racket/base

(require racket/class
         racket/math
         racket/provide
         "environment.rkt"
         "function.rkt"
         "object.rkt"
         "types.rkt"
         (for-syntax racket/base))

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

(define (op:delete v)
  (error 'TODO))

(define (op:void v)
  (get-value v)
  'undefined)

(define (op:typeof v)
  (if (and (reference? v)
           (eq? 'undefined (reference-base v)))
      "undefined"
      (let ([v (get-value v)])
        (cond
          [(eq? 'undefined v) "undefined"]
          [(eq? 'null v) "object"]
          [(boolean? v) "boolean"]
          [(number? v) "number"]
          [(string? v) "string"]
          [(is-a? v function%) "function"]
          [(object? v) "object"]))))

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

(define-values (op:+ op:- op:* op:/ op:%)
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
        (list + - * / modulo))))

(define-values (op:<< op:>> op:>>>)
  (apply
   values
   (map (λ (op)
          (λ (n m)
            (op (to-int32 (get-value n))
                (to-uint32 (get-value n)))))
        (list (λ (n m)
                (error 'TODO))
              (λ (n m)
                (error 'TODO))
              (λ (n m)
                (error 'TODO))))))

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
             [(eq? 'undefined a)
              (or (eq? 'undefined b)
                  (eq? 'null b))]
             [(eq? 'null a)
              (or (eq? 'null b)
                  (eq? 'undefined b))]
             [(boolean? a)
              (if (boolean? b)
                  (eq? a b)
                  (op:== (to-number a) b))]
             [(boolean? b) (op:== a (to-number b))]
             [(number? a)
              (cond
                [(number? b) (= a b)]
                [(string? b) (op:== a (to-number b))]
                [(object? b) (op:== a (to-primitive b))]
                [else #f])]
             [(string? a)
              (cond
                [(string? b) (string=? a b)]
                [(number? b) (op:== (to-number a) b)]
                [(object? b) (op:== a (to-primitive b))]
                [else #f])]
             [(object? a)
              (cond
                [(object? b) (eq? a b)]
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

(define-syntaxes (op:&& op:\|\|)
  (apply
   values
   (map (λ (op)
          (λ (stx)
            (syntax-case stx ()
              [(_ a b)
               #`(#,op (to-boolean (get-value a))
                       (to-boolean (get-value b)))])))
        (list #'and #'or))))

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
