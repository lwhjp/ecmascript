#lang racket/base

(require (only-in racket/class get-field is-a? send)
         "types.rkt"
         "private/error.rkt"
         "private/object.rkt")

(provide (all-defined-out))

(define (object? v)
  (is-a? v ecma-object%))

(define (get-property-value object name)
  (let ([property (get-property object name)])
    (cond
      [(data-property? property)
       (data-property-value property)]
      [(accessor-property? property)
       (let ([get (accessor-property-get property)])
         (if get
             (send get call object)
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
                  (send (accessor-property-set prop) call object v)
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
      [(not (get-field prototype object))
       (get-field extensible? object)]
      [else
       (let ([inherited (get-property (get-field prototype object) name)])
         (cond
           [(accessor-property? inherited)
            (if (accessor-property-set inherited) #t #f)]
           [(and (data-property? inherited) (get-field extensible? object))
            (data-property-writable? inherited)]
           [else (get-field extensible? object)]))])))

(define (delete-property! object name [throw? #t])
  (let ([prop (get-own-property object name)])
    (if prop
        (if (property-configurable? prop)
            (begin
              (hash-remove! (get-field properties object) name)
              #t)
            (and throw?
                 (raise-native-error
                  'type
                  (format "~a: not configurable" name))))
        #t)))
