#lang racket/base

(require (for-syntax racket/base
                     syntax/parse)
         (only-in racket/class new get-field))

(provide make-object
         extend-object)

(begin-for-syntax
  (define-syntax-class property-name
    #:attributes (str)
    [pattern str:str]
    [pattern
     name:id
     #:attr str (datum->syntax
                 #f
                 (symbol->string #'name)
                 #'name)]
    [pattern
     name:number
     #:attr str (datum->syntax
                 #f
                 (number->string #'name)
                 #'name)])
  (define-syntax-class property-clause
    #:attributes (name.str constructor)
    [pattern
     [name:property-name
      value:expr
      (~or (~optional (~seq #:configurable configurable?:boolean)
                      #:defaults ([configurable? #'#t]))
           (~optional (~seq #:enumerable enumerable?:boolean)
                      #:defaults ([enumerable? #'#f]))
           (~optional (~seq #:writable writable?:boolean)
                      #:defaults ([writable? #'#t]))) ...]
     #:attr constructor #'(data-property
                           configurable?
                           enumerable?
                           writable?
                           value)]
    [pattern
     [name:property-name
      (~or (~optional (~seq #:get getter:expr)
                      #:defaults ([getter #'#f]))
           (~optional (~seq #:set setter:expr)
                      #:defaults ([setter #'#f]))
           (~optional (~seq #:configurable configurable?:boolean)
                      #:defaults ([configurable? #'#t]))
           (~optional (~seq #:enumerable enumerable?:boolean)
                      #:defaults ([enumerable? #'#f]))) ...]
     #:attr constructor #'(accessor-property
                           configurable?
                           enumerable?
                           setter
                           getter)]))

(define-syntax make-object
  (syntax-parser
   [(_ (~optional class-name:str
                  #:defaults ([class-name "Object"]))
       (~optional proto-expr
                  #:defaults ([proto-expr #'object-prototype]))
       prop:property-clause ...)
    #'(new
       ecma-object%
       [class class-name]
       [prototype proto-expr]
       [initial-properties
        (list
         (cons prop.name.str
               prop.constructor) ...)])]))

(define-syntax extend-object
  (syntax-parser
   [(_ obj prop:property-clause ...)
    #`(hash-set*!
       (get-field properties obj)
       #,@(foldr
           list*
           '()
           (attribute prop.name.str)
           (attribute prop.constructor)))]))
