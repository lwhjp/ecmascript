#lang racket/base

(require (for-syntax racket/base
                     syntax/parse)
         (only-in racket/class get-field is-a? send)
         racket/stxparam
         "private/environment.rkt"
         "private/error.rkt"
         "private/function.rkt"
         "private/global-object.rkt"
         "private/this.rkt"
         "convert.rkt"
         "object.rkt"
         "types.rkt")

(provide (all-defined-out)
         Function?
         constructor?
         return
         function
 (rename-out
  [ecma:this this]))

(define (call ref . args)
  (let ([func (get-value ref)])
    (unless (Function? func)
      (raise-native-error 'type "not a function"))
    (let ([this-value
           (if (reference? ref)
               (let ([base (reference-base ref)])
                 (cond
                   [(Object? base) base]
                   [(is-a? base environment-record%)
                    (send base implicit-this-value)]))
               'undefined)])
      (let ([argvs (map get-value args)])
        (apply/this
         (cond
           [(or (null? this-value)
                (undefined? this-value))
            global-object]
           [(Object? this-value) this-value]
           [else (to-object this-value)])
         (get-field proc func)
         argvs)))))

(define (new ref . args)
  (let ([constructor (get-value ref)])
    (unless (constructor? constructor)
      (raise-native-error 'type "not a constructor"))
    (let ([argvs (map get-value args)])
      (apply (get-field new-proc constructor) argvs))))
