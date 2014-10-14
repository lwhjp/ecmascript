#lang racket/base

(require (for-syntax racket/base)
         racket/class)

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

(define ecma-object%
  (class object%
    (init-field prototype
                class
                [extensible? #t])

    (init [initial-properties '()])

    (field [properties (make-hash initial-properties)])

    (super-new)

    (define/public (get p)
      (let ([prop (send this get-own-property p)])
        (cond
          [(data-property? prop)
           (data-property-value prop)]
          [(accessor-property? prop)
           (let ([get (accessor-property-get prop)])
             (if get
                 (send get call this)
                 'undefined))]
          [else 'undefined])))

    (define/public (get-own-property p)
      (hash-ref properties p #f))

    (define/public (get-property p)
      (hash-ref properties p
                (λ ()
                  (and prototype
                       (send prototype get-property p)))))

    (define/public (put! p v [throw? #t])
      (if (send this can-put? p)
          (let ([own (send this get-own-property p)])
            (if (data-property? own)
                (send this define-own-property p `(data (value . ,v)) throw?)
                (let ([prop (send this get-property p)])
                  (if (accessor-property? prop)
                      (send (accessor-property-set prop) call this v)
                      (send this define-own-property
                            p
                            `(data
                              (value . ,v)
                              (writable . #t)
                              (enumerable . #t)
                              (configurable . #t))
                            throw?)))))
          (when throw?
            (error "type error"))))

    (define/public (can-put? p)
      (let ([prop (send this get-own-property p)])
        (cond
          [(accessor-property? prop)
           (if (accessor-property-set prop) #t #f)]
          [(data-property? prop)
           (data-property-writable? prop)]
          [(not prototype)
           extensible?]
          [else
           (let ([inherited (and prototype
                                 (send prototype get-property p))])
             (cond
               [(accessor-property? inherited)
                (if (accessor-property-set inherited) #t #f)]
               [(and (data-property? inherited) extensible?)
                (data-property-writable? inherited)]
               [else extensible?]))])))

    (define/public (has-property? p)
      (if (send this get-property p) #t #f))

    (define/public (delete! p [throw? #t])
      (let ([prop (send this get-own-property p)])
        (if prop
            (if (property-configurable? prop)
                (begin
                  (hash-remove! properties p)
                  #t)
                (and throw? (error "type error")))
            #t)))

    (define/public (default-value [hint 'number])
      (let/ec return
        (for ([method (if (eq? 'string hint)
                          '("toString" "valueOf")
                          '("valueOf" "toString"))])
          (let ([f (send this get method)])
            (when (and (is-a? f ecma-object%)
                       (object-method-arity-includes? f 'call 1))
              (let ([v (send f call this)])
                (unless (is-a? v ecma-object%)
                  (return v))))))
        (error "type error")))

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

    (define/public (define-own-property p desc throw?)
      (let ([reject (λ () (and throw? (error "type error")))]
            [current (send this get-own-property p)])
        (cond
          [(not current)
           (if (not extensible?)
               (reject)
               (begin
                 (hash-set!
                  properties
                  p
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
                (or (property-configurable? desc)
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
                 (hash-set! properties p current)
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
               (update-property current desc))])))))

(define-syntax-rule (define-object-properties obj [prop val] ...)
  (begin
    (hash-set! (get-field properties obj)
               prop
               (make-data-property val)) ...))

(define object-prototype
  (new ecma-object%
       [prototype #f]
       [class "Object"]))
