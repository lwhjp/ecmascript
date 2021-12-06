#lang typed/racket/base

(require typed/racket/class
         "environment.rkt"
         "object.rkt"
         "primitive.rkt")

(provide (all-defined-out))

(struct realm
  ([intrinsics : (HashTable Symbol ESObject)]
   [global-object : ESObject]
   [global-env : ESGlobalEnvironment]
   ; TODO: templates
   [host-defined : Any])
  #:type-name ESRealm)

(define (make-realm [intrinsics : (HashTable Symbol ESObject)]
                    [global-object : (U ESObject ESUndefined) es-undefined]
                    [this-value : (U ESObject ESUndefined) es-undefined])
  (let* ([global-object (if (es-undefined? global-object)
                            (new es-object%
                                 [class-name 'global]
                                 [prototype (hash-ref intrinsics '%Object.prototype%)])
                            global-object)]
         [this-value (if (es-undefined? this-value)
                         global-object
                         this-value)])
    (realm intrinsics
           global-object
           (new-global-environment global-object this-value)
           es-undefined)))

(define current-realm
  : (Parameterof (Option ESRealm))
  (make-parameter #f))

(define (get-global-object)
  (realm-global-object (assert (current-realm))))

; TODO: remove

(define (clone-realm [r : ESRealm])
  (make-realm (realm-intrinsics r)
              (send (realm-global-object r) clone)))
