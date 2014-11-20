#lang racket/base

(require "error.rkt")

(provide (all-defined-out))

(struct property (configurable? enumerable?) #:mutable #:transparent)

(struct data-property property (writable? value) #:mutable #:transparent)

(define (make-data-property [value 'undefined]
                            #:writable [writable? #f]
                            #:enumerable [enumerable? #f]
                            #:configurable [configurable? #f])
  (data-property configurable? enumerable? writable? value))

(struct accessor-property property (set get) #:mutable #:transparent)

(define (make-accessor-property #:get [get #f]
                                #:set [set #f]
                                #:enumerable [enumerable? #f]
                                #:configurable [configurable? #f])
  (accessor-property configurable? enumerable? set get))

(define-values (prop:class has-class? get-class)
  (make-struct-type-property 'class))

(struct Object
  (prototype
   properties
   [extensible? #:mutable])
  #:constructor-name Object
  #:property prop:class 'Object)

(define (get-property object name)
  (hash-ref
   (Object-properties object)
   name
   (λ ()
     (let ([prototype (Object-prototype object)])
       (and prototype
            (get-property prototype name))))))

(define (get-own-property object name)
  (hash-ref
   (Object-properties object)
   name
   #f))

(define (define-own-property object name desc throw?)
  (define (generic-desc? desc)
    (and (not (data-desc? desc))
         (not (accessor-desc? desc))))
  (define (data-desc? desc)
    (eq? 'data (car desc)))
  (define (accessor-desc? desc)
    (eq? 'accessor (car desc)))
  (define (desc-get desc v def)
    (let ([p (assq v (cdr desc))])
      (if p (cdr p) def)))
  (define (update-property p desc)
    (let loop ([fields (cdr desc)])
      (unless (null? fields)
        (case (caar fields)
          [(configurable) (set-property-configurable?! p (cdar fields))]
          [(enumerable) (set-property-enumerable?! p (cdar fields))]
          [(writable) (set-data-property-writable?! p (cdar fields))]
          [(value) (set-data-property-value! p (cdar fields))]
          [(set) (set-accessor-property-set! p (cdar fields))]
          [(get) (set-accessor-property-get! p (cdar fields))])
        (loop (cdr fields)))))
  (let ([reject (λ () (and throw? (raise-native-error 'type)))]
        [current (get-own-property object name)])
    (cond
      [(not current)
       (if (not (Object-extensible? object))
           (reject)
           (begin
             (hash-set!
              (Object-properties object)
              name
              (if (accessor-desc? desc)
                  (make-accessor-property #:get (desc-get desc 'get 'undefined)
                                          #:set (desc-get desc 'set 'undefined)
                                          #:enumerable (desc-get desc 'enumerable #f)
                                          #:configurable (desc-get desc 'configurable #f))
                  (make-data-property (desc-get desc 'value 'undefined)
                                      #:writable (desc-get desc 'writable #f)
                                      #:enumerable (desc-get desc 'enumerable #f)
                                      #:configurable (desc-get desc 'configurable #f))))
             #t))]
      [(null? (cdr desc)) #t]
      [#f #t] ; TODO: step 6
      [(and (not (property-configurable? current))
            (or (desc-get desc 'configurable #f)
                (and (memv 'enumerable (cdr desc))
                     (not (eq? (property-enumerable? current)
                               (memv 'enumerable (cdr desc)))))))
       (reject)]
      [(generic-desc? desc)
       (update-property current desc)]
      [(and (not (eq? (data-property? current)
                      (data-desc? desc))))
       (if (property-configurable? current)
           (let ([current
                  (if (data-property? current)
                      (make-accessor-property #:configurable (property-configurable? current)
                                              #:enumerable (property-enumerable? current))
                      (make-data-property #:configurable (property-configurable? current)
                                          #:enumerable (property-enumerable? current)))])
             (hash-set! (Object-properties object) name current)
             (update-property current desc))
           (reject))]
      [(and (data-property? current) (data-desc? desc))
       (if (and (not (property-configurable? current))
                (and (not (data-property-writable? current))
                     (or (desc-get desc 'writable #f)
                         #| TODO: same-value |#)))
           (reject)
           (update-property current desc))]
      [else
       (if (and (not (property-configurable? current))
                (or (and (desc-get desc 'set 'undefined)
                         #| TODO |#)
                    (and (desc-get desc 'get 'undefined)
                         #| TODO |#)))
           (reject)
           (update-property current desc))])))

(define-syntax-rule (define-object-properties obj [prop val] ...)
  (begin
    (hash-set! (Object-properties obj)
               prop
               (make-data-property val
                                   #:writable #t
                                   #:enumerable #f
                                   #:configurable #t)) ...))

(define Object:prototype
  (Object #f (make-hash) #t))

(define (from-property-descriptor desc)
  (if desc
      (let ([obj (Object Object:prototype (make-hash) #t)])
        (if (data-property? desc)
            (begin
              (define-own-property obj "value"
                    `(data (value . ,(data-property-value desc))
                           (writable . #t)
                           (enumerable . #t)
                           (configurable . #t))
                    #f)
              (define-own-property obj "writable"
                    `(data (value . ,(data-property-writable? desc))
                           (writable . #t)
                           (enumerable . #t)
                           (configurable . #t))
                    #f))
            (begin
              (define-own-property obj "get"
                    `(data (value . ,(accessor-property-get desc))
                           (writable . #t)
                           (enumerable . #t)
                           (configurable . #t))
                    #f)
              (define-own-property obj "set"
                    `(data (value . ,(accessor-property-set desc))
                           (writable . #t)
                           (enumerable . #t)
                           (configurable . #t))
                    #f)))
        (define-own-property obj "enumerable"
              `(data (value . ,(property-enumerable? desc))
                     (writable . #t)
                     (enumerable . #t)
                     (configurable . #t))
              #f)
        (define-own-property obj "configurable"
              `(data (value . ,(property-configurable? desc))
                     (writable . #t)
                     (enumerable . #t)
                     (configurable . #t))
              #f)
        obj)
      'undefined))

(define (to-property-descriptor obj)
  (unless (Object? obj)
    (raise-native-error 'type "not an object"))
  (let ([oprops (Object-properties obj)])
    (define-values (enumerable? configurable?
                    value writable? get set)
      (apply
       values
       (map
        (λ (name)
          (hash-ref oprops name 'undefined))
        '("enumerable" "configurable"
          "value" "writable" "get" "set"))))
    (define-values (kind attrs)
      (cond
        [(or (not (eq? 'undefined value))
             (not (eq? 'undefined writable?)))
         (values 'data
                 (append
                  (if (eq? 'undefined value) '() `((value . ,value)))
                  (if (eq? 'undefined writable?) '() `((writable . ,writable?)))))]
        [else
         (values 'accessor
                 (append
                  (if (eq? 'undefined get) '() `((get . ,get)))
                  (if (eq? 'undefined set) '() `((set . ,set)))))]))
    (cons kind
          (append attrs
                  (if (eq? 'undefined enumerable?) '() `((enumerable . ,enumerable?)))
                  (if (eq? 'undefined configurable?) '() `((configurable . ,configurable?)))))))
