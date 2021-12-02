#lang racket/base

(require (for-syntax racket/base)
         racket/class
         "../convert.rkt"
         "../private/environment.rkt"
         "../private/error.rkt"
         "../private/function.rkt"
         "../private/object.rkt"
         "../private/primitive.rkt"
         "../private/realm.rkt"
         "../private/string.rkt"
         "environment.rkt")

(provide
 identifier-reference
 set-reference!
 update-reference!
 post-update-reference!
 identifier
 member
 delete
 typeof
 call)

(define-syntax (#%ref stx)
  (syntax-case stx ()
    [(_ expr) #'(get-value expr)]))

(define-for-syntax (preserve-reference stx)
  (syntax-case (local-expand stx 'expression (list #'#%ref)) (#%ref)
    [(#%ref v) #'v]
    [v #'v]))

(define-syntax (identifier-reference stx)
  (syntax-case stx ()
    [(_ id)
     (identifier? #'id)
     (preserve-reference #'(identifier id))]))

(define-syntax (set-reference! stx)
  (syntax-case stx ()
    [(_ ref-expr v)
     (with-syntax ([ref (preserve-reference #'ref-expr)])
       #'(put-value! ref v))]))

(define-syntax (update-reference! stx)
  (syntax-case stx ()
    [(_ ref-expr proc)
     (with-syntax ([ref (preserve-reference #'ref-expr)])
       #'(let* ([r ref]
                [v (proc (get-value r))])
           (put-value! r v)
           v))]))

(define-syntax (post-update-reference! stx)
  (syntax-case stx ()
    [(_ ref-expr proc)
     (with-syntax ([ref (preserve-reference #'ref-expr)])
       #'(let* ([r ref]
                [v (get-value r)])
           (put-value! r (proc v))
           v))]))

(define-syntax (identifier stx)
  (syntax-case stx ()
    [(_ id)
     (identifier? #'id)
     (with-syntax ([name (symbol->string (syntax-e #'id))])
       #'(#%ref
          (get-identifier-reference
           lexical-environment
           name
           #f)))]))

(define-syntax (member stx)
  (syntax-case stx ()
    [(_ base prop)
     (with-syntax ([prop-name (if (identifier? #'prop)
                                  (symbol->string (syntax-e #'prop))
                                  #'(es-string->string (to-string prop)))])
       #'(#%ref (reference (to-object base) prop-name #f)))]))

(define (delete/ref ref)
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

(define-syntax (delete stx)
  (syntax-case stx ()
    [(_ expr)
     (with-syntax ([ref (preserve-reference #'expr)])
       #'(delete/ref ref))]))

(define (typeof/value v)
   (cond
     [(ecma:undefined? v) "undefined"]
     [(ecma:null? v) "object"]
     [(boolean? v) "boolean"]
     [(number? v) "number"]
     [(string? v) "string"]
     [(Function? v) "function"]
     [(Object? v) "object"]))

(define-syntax (typeof stx)
  (syntax-case stx ()
    [(_ expr)
     (with-syntax ([ref-expr (preserve-reference #'expr)])
       #'(let ([v ref-expr])
           (typeof/value
            (if (reference? v)
                (if (ecma:undefined? (reference-base v))
                    ecma:undefined
                    (get-value v))
                v))))]))

(define (call/reference ref . args)
  (define func (get-value ref))
  (unless (Function? func)
    (raise-native-error 'type "not a function"))
  (define this-value
    (if (reference? ref)
        (let ([base (reference-base ref)])
          (cond
            [(Object? base) base]
            [(is-a? base environment-record%)
             (send base implicit-this-value)]))
        ecma:undefined))
  (send func call
        (cond
          [(or (ecma:null? this-value)
               (ecma:undefined? this-value))
           (current-global-object)]
          [(Object? this-value) this-value]
          [else (to-object this-value)])
        args))

(define-syntax (call stx)
  (syntax-case stx ()
    [(_ func-expr arg ...)
     (with-syntax ([func-ref (preserve-reference #'func-expr)])
       #'(call/reference func-ref arg ...))]))
