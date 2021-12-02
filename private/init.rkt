#lang racket/base

(require (only-in racket/class get-field new send)
         racket/match
         racket/math
         racket/promise
         racket/runtime-path
         net/uri-codec
         "environment.rkt"
         (prefix-in ecma: "eval.rkt")
         "object.rkt"
         "primitive.rkt"
         "realm.rkt"
         "string.rkt"
         "this.rkt"
         (only-in "../lib/util.rkt" native-method)
         "../convert.rkt"
         "../types.rkt")

(provide make-realm)

(define make-realm
  (let ([prototype
         (lazy
          (define global-object
            (new ecma-object%
                 [class-name 'global]
                 [prototype #f]))
          (define global-environment
            (new-object-environment global-object ecma:null))
          (parameterize
              ([current-realm
                (new realm%
                     [global-object global-object]
                     [global-environment global-environment])])
            (set-default-global-bindings!)
            (current-realm)))])
    (λ () (send (force prototype) clone))))

(define-runtime-module-path lib:array "../lib/array.rkt")
(define-runtime-module-path lib:boolean "../lib/boolean.rkt")
(define-runtime-module-path lib:error "../lib/error.rkt")
(define-runtime-module-path lib:function "../lib/function.rkt")
(define-runtime-module-path lib:math "../lib/math.rkt")
(define-runtime-module-path lib:number "../lib/number.rkt")
(define-runtime-module-path lib:object "../lib/object.rkt")
(define-runtime-module-path lib:regexp "../lib/regexp.rkt")
(define-runtime-module-path lib:string "../lib/string.rkt")

(define (import-library mod)
  (define imported-properties
    ((dynamic-require mod 'get-properties)))
  (define global-property-map
    (get-field properties (current-global-object)))
  (for ([prop (in-list imported-properties)])
    (match-define (cons name value) prop)
    (hash-set! global-property-map
               name
               (if (property? value)
                   value
                   (make-data-property
                    value
                    #:writable #t
                    #:enumerable #f
                    #:configurable #f)))))

(define (set-default-global-bindings!)
  (define global-object (current-global-object))
  (parameterize
      ([current-ecma:this global-object])
    (define-object-properties global-object
      ["Infinity" +inf.0]
      ["NaN" +nan.0]
      ["undefined" ecma:undefined]
      ["eval"
       (native-method (x)
         (if (es-string? x)
             (ecma:eval (es-string->string x))
             x))]
      ["parseInt"
       (native-method (string radix)
         (string->number (to-string string)
                         (to-int32 radix)))]
      ["parseFloat"
       (native-method (string)
         (string->number (to-string string)))]
      ["isNaN"
       (native-method (number)
         (nan? (to-number number)))]
      ["isFinite"
       (native-method (number)
         (not (infinite? number)))]
      ["decodeURI"
       (native-method (encodedURI)
         (uri-decode (to-string encodedURI)))]
      ["decodeURIComponent"
       (native-method (encodedURIComponent)
         (uri-path-segment-decode (to-string encodedURIComponent)))]
      ["encodeURI"
       (native-method (uri)
         (uri-encode (to-string uri)))]
      ["encodeURIComponent"
       (native-method (uriComponent)
         (uri-path-segment-encode (to-string uriComponent)))])

    (for-each import-library
              (list lib:array
                    lib:boolean
                    lib:error
                    lib:function
                    lib:math
                    lib:number
                    lib:object
                    lib:regexp
                    lib:string))

    (define prelude "
Array.prototype.pop=(function(){
                                if(this.length===0){
                                                    return undefined;
                                                           }
                                  var i=this.length-1;
                                  var ret=this[i];
                                  delete this[i];
                                  this.length-=1;
                                  return ret;
                                  });
Array.prototype.push=(function(x){
         this[this.length]=x;
});
Array.prototype.shift=(function(){
    var ret=this[0];
    for(var i=1;i<this.length;i++){
        this[i-1]=this[i];
    }
    this.length-=1;
    return ret;
});
Array.prototype.unshift=(function(x){
    var len=this.length;
    for(var i=len-1;i>=0;i--){
        this[i+1]=this[i];
    }
    this[0]=x;
});
")
    (void (ecma:eval prelude))))
