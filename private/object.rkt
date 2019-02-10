#lang racket/base

(require racket/class
         "error.rkt"
         "primitive.rkt"
         (only-in "this.rkt" apply/this))

(provide (all-defined-out))

(struct property (configurable? enumerable?) #:mutable #:transparent)

(struct data-property property (writable? value) #:mutable #:transparent)

(define (make-data-property [value ecma:undefined]
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

(define ecma-object%
  (class object%
    (init-field class-name
                prototype
                [properties (make-hash)]
                [extensible? #t])
    (super-new)
    (define/public (get-own-property name)
      (hash-ref properties name #f))
    (define/public (get-property name)
      (cond
        [(get-own-property name)]
        [prototype (send prototype get-property name)]
        [else #f]))
    (define/public (get name)
      (define property (get-property name))
      (cond
        [(data-property? property)
         (data-property-value property)]
        [(accessor-property? property)
         (let* ([get (accessor-property-get property)]
                [get (if (procedure? get) get (get-field proc get))])
           (if get
               (apply/this this get '())
               ecma:undefined))]
        [else ecma:undefined]))
    (define/public (can-put? name)
      (define prop (get-own-property name))
      (cond
        [(accessor-property? prop) (if (accessor-property-set prop) #t #f)]
        [(data-property? prop) (data-property-writable? prop)]
        [(not prototype) extensible?]
        [else
         (let ([inherited (send prototype get-property name)])
           (cond
             [(accessor-property? inherited)
              (if (accessor-property-set inherited) #t #f)]
             [(and (data-property? inherited) extensible?)
              (data-property-writable? inherited)]
             [else extensible?]))]))
    (define/public (put name v [throw? #t])
      (cond
        [(can-put? name)
         (cond
           [(data-property? (get-own-property name))
            (define-own-property
              name
              `(data (value . ,v))
              throw?)]
           [else
            (let ([prop (get-property name)])
              (if (accessor-property? prop)
                  (let* ([set (accessor-property-set prop)]
                         [set (if (procedure? set) set (get-field proc set))])
                    (apply/this this set (list v)))
                  (define-own-property
                    name
                    `(data
                      (value . ,v)
                      (writable . #t)
                      (enumerable . #t)
                      (configurable . #t))
                    throw?)))])]
        [throw? (raise-native-error 'type (format "~a: can't set property" name))]
        [else (void)]))
    (define/public (has-property? name)
      (and (get-property name) #t))
    (define/public (delete name [throw? #t])
      (define prop (get-own-property name))
      (cond
        [(not prop) #t]
        [(property-configurable? prop)
         (begin
           (hash-remove! properties name)
           #t)]
        [throw? (raise-native-error 'type (format "~a: not configurable" name))]
        [else #f]))
    (define/public (define-own-property name desc [throw? #t])
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
            [current (get-own-property name)])
        (cond
          [(not current)
           (if (not extensible?)
               (reject)
               (begin
                 (hash-set!
                  properties
                  name
                  (if (accessor-desc? desc)
                      (make-accessor-property #:get (desc-get desc 'get ecma:undefined)
                                              #:set (desc-get desc 'set ecma:undefined)
                                              #:enumerable (desc-get desc 'enumerable #f)
                                              #:configurable (desc-get desc 'configurable #f))
                      (make-data-property (desc-get desc 'value ecma:undefined)
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
                 (hash-set! properties name current)
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
                    (or (and (desc-get desc 'set ecma:undefined)
                             #| TODO |#)
                        (and (desc-get desc 'get ecma:undefined)
                             #| TODO |#)))
               (reject)
               (update-property current desc))])))))

(define Object%
  (class ecma-object%
    (init [prototype Object:prototype])
    (super-new [class-name 'Object]
               [prototype prototype])))

(define (Object? v) (is-a? v ecma-object%))

(define (get-property object name)
  (send object get-property name))

(define (get-own-property object name)
  (send object get-own-property name))

(define (define-own-property object name desc throw?)
  (send object define-own-property name desc throw?))

(define-syntax-rule (define-object-properties obj [prop val] ...)
  (begin
    (hash-set! (get-field properties obj)
               prop
               (make-data-property val
                                   #:writable #t
                                   #:enumerable #f
                                   #:configurable #t)) ...))

(define Object:prototype
  (new Object% [prototype #f]))

(define (from-property-descriptor desc)
  (if desc
      (let ([obj (new Object%)])
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
      ecma:undefined))

(define (to-property-descriptor obj)
  (unless (Object? obj)
    (raise-native-error 'type "not an object"))
  (let ([oprops (get-field properties obj)])
    (define-values (enumerable? configurable?
                    value writable? get set)
      (apply
       values
       (map
        (λ (name)
          (hash-ref oprops name ecma:undefined))
        '("enumerable" "configurable"
          "value" "writable" "get" "set"))))
    (define-values (kind attrs)
      (cond
        [(or (not (ecma:undefined? value))
             (not (ecma:undefined? writable?)))
         (values 'data
                 (append
                  (if (ecma:undefined? value) '() `((value . ,value)))
                  (if (ecma:undefined? writable?) '() `((writable . ,writable?)))))]
        [else
         (values 'accessor
                 (append
                  (if (ecma:undefined? get) '() `((get . ,get)))
                  (if (ecma:undefined? set) '() `((set . ,set)))))]))
    (cons kind
          (append attrs
                  (if (ecma:undefined? enumerable?) '() `((enumerable . ,enumerable?)))
                  (if (ecma:undefined? configurable?) '() `((configurable . ,configurable?)))))))
