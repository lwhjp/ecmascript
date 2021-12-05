#lang racket/base

(require racket/class
         "../lang/helpers.rkt"
         "error.rkt"
         "object.rkt"
         "primitive.rkt"
         "string.rkt"
         "this.rkt")

(provide (all-defined-out))

(define ecma-function%
  (class ecma-object%
    (init-field formal-parameters proc)
    (super-new [class-name 'Function])
    (define/public (bind-arguments args env)
      (for ([arg-sym (in-list formal-parameters)]
            [v (in-sequences (in-list args)
                             (in-cycle (list ecma:undefined)))])
        (define arg-name (string->es-string (symbol->string arg-sym)))
        (unless (send env has-binding? arg-name)
          (send env create-mutable-binding! arg-name #f))
        (send env initialize-binding! arg-name v))
      (define arguments-name (es-string-literal "arguments"))
      (unless (send env has-binding? arguments-name)
        (send env create-immutable-binding! arguments-name #f))
      (send env initialize-binding!
            arguments-name
            (create-arguments-object args env)))
    (define/public (call this-arg args)
      (apply/this this-arg proc args))
    (abstract construct)
    (abstract create-arguments-object)
    (define-own-property
      this
      "length"
      `(data
        (value . ,(length formal-parameters))
        (writable . #f)
        (enumerable . #f)
        (configurable . #f))
      #f)))

(define (Function? v) (is-a? v ecma-function%))

(define (es-function? v) (Function? v))

(define (call/function f this-arg . args)
  (send f call this-arg args))

(define (has-instance? f v)
  (and
   (Object? v)
   (let ([o (get-property-value f "prototype")])
     (unless (Object? o)
       (raise-native-error 'type "not an object"))
     (let loop ([v v])
       (let ([v (get-field prototype v)])
         (and
          v
          (or (eq? o v)
              (loop v))))))))

(define arguments%
  (class ecma-object%
    (init func args env)
    (super-new [class-name 'Arguments])
    (let ([formal-parameters (get-field formal-parameters func)])
      (for ([arg-name (in-sequences (in-list (map symbol->string formal-parameters))
                                    (in-cycle (list #f)))]
            [arg-v (in-sequences (in-list args)
                                 (in-cycle (list ecma:undefined)))]
            [index (in-range (max (length formal-parameters)
                                  (length args)))])
        (define-own-property
          this
          (number->string index)
          `(data (value . ,arg-v)
                 (writable . #t)
                 (enumerable . #t)
                 (configurable . #t))
          #f)
        (when arg-name
          (define-own-property
            this
            arg-name
            `(accessor (get . ,(λ () (send env get-binding-value arg-name #t)))
                       (set . ,(λ (v) (send env set-mutable-binding! arg-name v #t)))
                       (configurable . #t))
            #f))))
    (define-own-property
          this
          "length"
          `(data (value . ,(length args))
                 (writable . #t)
                 (enumerable . #f)
                 (configurable . #t))
          #f)
    (define-own-property
          this
          "callee"
          `(data (value . ,func)
                 (writable . #t)
                 (enumerable . #f)
                 (configurable . #t))
          #f)))
