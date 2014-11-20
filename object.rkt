#lang racket/base

(require "types.rkt"
         "private/error.rkt"
         "private/object.rkt"
         "private/this.rkt")

(provide (all-defined-out)
         Object?)

(define (get-property-value object name)
  (let ([property (get-property object name)])
    (cond
      [(data-property? property)
       (data-property-value property)]
      [(accessor-property? property)
       (let ([get (accessor-property-get property)])
         (if get
             (apply/this object get '())
             undefined))]
      [else undefined])))

(define (has-property? object name)
  (and (get-property object name) #t))

(define (set-property-value! object name v [throw? #t])
  (if (can-set-property? object name)
      (let ([own (get-own-property object name)])
        (if (data-property? own)
            (define-own-property
             object
             name
             `(data (value . ,v))
             throw?)
            (let ([prop (get-property object name)])
              (if (accessor-property? prop)
                  (apply/this object (accessor-property-set prop) (list v))
                  (define-own-property
                   object
                   name
                   `(data
                     (value . ,v)
                     (writable . #t)
                     (enumerable . #t)
                     (configurable . #t))
                   throw?)))))
      (when throw?
        (raise-native-error
         'type
         (format "~a: can't set property" name)))))

(define (can-set-property? object name)
  (let ([prop (get-own-property object name)])
    (cond
      [(accessor-property? prop)
       (if (accessor-property-set prop) #t #f)]
      [(data-property? prop)
       (data-property-writable? prop)]
      [(not (Object-prototype object))
       (Object-extensible? object)]
      [else
       (let ([inherited (get-property (Object-prototype object) name)])
         (cond
           [(accessor-property? inherited)
            (if (accessor-property-set inherited) #t #f)]
           [(and (data-property? inherited) (Object-extensible? object))
            (data-property-writable? inherited)]
           [else (Object-extensible? object)]))])))

(define (delete-property! object name [throw? #t])
  (let ([prop (get-own-property object name)])
    (if prop
        (if (property-configurable? prop)
            (begin
              (hash-remove! (Object-properties object) name)
              #t)
            (and throw?
                 (raise-native-error
                  'type
                  (format "~a: not configurable" name))))
        #t)))
