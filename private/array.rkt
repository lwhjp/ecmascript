#lang typed/racket/base

(require typed/racket/class
         racket/math
         "convert.rkt"
         "error.rkt"
         "initializable.rkt"
         "object.rkt"
         "primitive.rkt"
         "realm.rkt"
         "string.rkt"
         "unsafe-predicate.rkt")

(provide (all-defined-out))

(define es:length (string->es-string "length"))

(define-type ESArray<%>
  (Class #:implements/inits ESObject<%>))

(define-type ESArray (Instance ESArray<%>))

(define es-array%
  : ESArray<%>
  (class es-object%
    (super-new)
    (define/override (define-own-property! p desc)
      (cond
        [(equal? es:length p)
         (array-set-length! this desc)]
        [(array-index? p)
         (let ([old-len-desc (ordinary-get-own-property this es:length)])
           (assert old-len-desc data-property?)
           (assert (not (property-configurable? old-len-desc)))
           (define old-len (unbox (assert (data-property-value old-len-desc) initialized?)))
           (assert old-len nonnegative-integer?)
           (define index (to-uint32 p))
           (cond
             [(and (>= index old-len)
                   (not (data-property-writable? old-len-desc)))
              #f]
             [(not (ordinary-define-own-property! this p desc))
              #f]
             [else
              (when (>= index old-len)
                (set-data-property-value! old-len-desc (box (+ index 1.0)))
                (ordinary-define-own-property! this es:length old-len-desc))
              #t]))]
        [else
         (ordinary-define-own-property! this p desc)]))))

(define-unsafe-class-predicate es-array? es-array% ESArray<%>)

(define (array-create [length : Exact-Nonnegative-Integer]
                      [proto : ESObject (get-intrinsic '%Array.prototype%)])
  (when (> length (sub1 (expt 2 32)))
    (raise-native-error 'range))
  (let ([a (new es-array%
                [class-name 'Array]
                [prototype proto])])
    (ordinary-define-own-property!
     a
     es:length
     (make-data-property (exact->inexact length)
                         #:writable? #t
                         #:enumerable? #f
                         #:configurable? #f))
    a))

; array-species-create

(define (array-set-length! [a : ESArray] [desc : ESProperty])
  : Boolean
  (let/ec return : Boolean
    (when (or (not (data-property? desc))
              (uninitialized? (data-property-value desc)))
      (return (ordinary-define-own-property! a es:length desc)))
    (define new-len-desc (property-copy desc))
    (define new-len (to-uint32 (unbox (assert (data-property-value desc) initialized?))))
    (define number-len (to-number (data-property-value desc)))
    (unless (same-value-zero? new-len number-len)
      (raise-native-error 'range))
    (set-data-property-value! new-len-desc (box new-len))
    (define old-len-desc (ordinary-get-own-property a es:length))
    (assert (data-property? old-len-desc))
    (assert (not (property-configurable? old-len-desc)))
    (define old-len (unbox (assert (data-property-value old-len-desc) initialized?)))
    (assert old-len es-number?)
    (when (>= new-len old-len)
      (return (ordinary-define-own-property! a es:length new-len-desc)))
    (unless (data-property-writable? old-len-desc)
      (return #f))
    (define new-writable?
      (if (not (eq? #f (data-property-writable? new-len-desc)))
          #t
          (begin
            (set-data-property-writable?! new-len-desc #t)
            #f)))
    (unless (ordinary-define-own-property! a es:length new-len-desc)
      (return #f))
    (for ([p (in-list
              ((inst sort ESString Nonnegative-Integer)
               (filter array-index? (send a own-property-keys))
               >
               #:key (λ ([v : ESString])
                       (cast (string->number (es-string->string v)) Nonnegative-Integer))
               #:cache-keys? #t))])
      (unless (send a delete! p)
        (set-data-property-value! new-len-desc (box (+ (to-uint32 p) 1.0)))
        (unless new-writable?
          (set-data-property-writable?! new-len-desc #f))
        (ordinary-define-own-property! a es:length new-len-desc)
        (return #f)))
    (unless new-writable?
      (ordinary-define-own-property! a es:length (make-data-property #:writable? #f)))
    #t))
