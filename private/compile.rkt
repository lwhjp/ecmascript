#lang racket/base

(require racket/list
         racket/match
         (prefix-in ecma: "../ast.rkt"))

(provide ecmascript->racket)

(define (ecmascript->racket stx)
  (syntax->list
   (compile-program stx)))

(define (extract-statement-vars stmt)
  (match stmt
    [(ecma:stmt:vars _ (list (ecma:decl:var _ id _) ...))
     id]
    [_ #f]))

(define (extract-vars elts)
  (apply
   append
   (filter-map
    extract-statement-vars
    elts)))

(define (extract-functions elts)
  (filter-map
   (match-lambda
     [(ecma:decl:fn _ def) def]
     [_ #f])
   elts))

(define (add-prefix pref sym)
  (string->symbol
   (string-append (symbol->string pref)
                  (symbol->string sym))))

(define (compile-expression stx)
  (match stx
    [(ecma:expr:this loc) (datum->syntax #f 'this loc)]
    [(ecma:expr:id loc symbol) (datum->syntax #f `(id ,symbol) loc)]
    [(ecma:expr:null loc) (datum->syntax #f 'null loc)]
    [(ecma:expr:bool loc v) (datum->syntax #f v loc)]
    [(ecma:expr:number loc v) (datum->syntax #f v loc)]
    [(ecma:expr:string loc v) (datum->syntax #f v loc)]
    [(ecma:expr:array loc elements)
     (datum->syntax #f
       `(array
         ,@(map (λ (elt)
                  (if elt
                      (compile-expression elt)
                      'undefined))
                elements))
       loc)]
    [(ecma:expr:object loc props)
     (datum->syntax #f
       `(object
         ,@(map compile-object-property props))
       loc)]
    [(ecma:expr:fn loc def) (compile-function def)]
    [(ecma:expr:member loc obj prop)
     (datum->syntax #f
       `(member ,(compile-expression obj)
                ,(if (symbol? prop)
                     (datum-intern-literal
                      (symbol->string prop))
                     (compile-expression prop)))
       loc)]
    [(ecma:expr:new loc expr args)
     (datum->syntax #f
       `(new ,(compile-expression expr)
             ,@(map compile-expression args))
       loc)]
    [(ecma:expr:call loc expr args)
     (datum->syntax #f
       `(call ,(compile-expression expr)
              ,@(map compile-expression args))
       loc)]
    [(ecma:expr:postfix loc expr op)
     (datum->syntax #f
       `(,(add-prefix 'post op)
         ,(compile-expression expr))
       loc)]
    [(ecma:expr:unary loc op expr)
     (datum->syntax #f
       `(,op ,(compile-expression expr))
       loc)]
    [(ecma:expr:binary loc left op right)
     (datum->syntax #f
       `(,op ,(compile-expression left)
             ,(compile-expression right))
       loc)]
    [(ecma:expr:cond loc test true false)
     (datum->syntax #f
       `(?: ,(compile-expression test)
            ,(compile-expression true)
            ,(compile-expression false))
       loc)]
    [(ecma:expr:comma loc left right)
     (datum->syntax #f
       `(\, ,(compile-expression left)
            ,(compile-expression right))
       loc)]))

(define (compile-object-property stx)
  (match stx
    [(ecma:init:obj:prop loc name value)
     (datum->syntax #f
       `[,name ,(compile-expression value)]
       loc)]
    [(ecma:init:obj:get loc prop fn)
     (datum->syntax #f
       `[,prop #:get ,(compile-function fn)]
       loc)]
    [(ecma:init:obj:set loc prop fn)
     (datum->syntax #f
       `[,prop #:set ,(compile-function fn)]
       loc)]))

(define (compile-statement stx)
  (match stx
    [(ecma:stmt:block loc stmts)
     (datum->syntax #f
       `(block ,@(map compile-statement stmts))
       loc)]
    [(ecma:stmt:vars loc decls)
     (datum->syntax #f
       `(block ,@(map compile-variable-declaration decls)))]
    [(ecma:stmt:empty loc)
     (datum->syntax #f
       '(empty-statement)
       loc)]
    [(ecma:stmt:expr loc expr)
     (datum->syntax #f
       `(get-value ,(compile-expression expr))
       loc)]
    [(ecma:stmt:if loc test true false)
     (datum->syntax #f
       (if false
           `(if ,(compile-expression test)
                ,(compile-statement true)
                ,(compile-statement false))
           `(if ,(compile-expression test)
                ,(compile-statement true)))
       loc)]
    [(ecma:stmt:while loc test body)
     (datum->syntax #f
       `(while ,(compile-expression test)
          ,(compile-statement body))
       loc)]
    [(ecma:stmt:for loc (list var-decl ...) test step body)
     (error 'TODO-vars)]
    [(ecma:stmt:for loc init test update body)
     (datum->syntax #f
       `(for #:init ,(and init
                          (compile-expression init))
             #:test ,(and test
                          (compile-expression test))
             #:update ,(and update
                            (compile-expression update))
          ,(compile-statement body)))]
    [(ecma:stmt:for-in loc i expr body)
     (error 'TODO-for-in)]
    [(ecma:stmt:continue loc label)
     (datum->syntax #f
       (if label
           `(continue ,label)
           '(continue))
       loc)]
    [(ecma:stmt:break loc label)
     (datum->syntax #f
       (if label
           `(break ,label)
           '(continue)))]
    [(ecma:stmt:return loc expr)
     (datum->syntax #f
       (if expr
           `(return ,(compile-expression expr))
           '(return))
       loc)]
    [(ecma:stmt:with loc expr body)
     (datum->syntax #f
       `(with ,(compile-expression expr)
          ,(compile-statement body))
       loc)]
    [(ecma:stmt:throw loc expr)
     (datum->syntax #f
       `(throw ,(compile-expression expr))
       loc)]
    [(ecma:stmt:try loc tb cid cb fb)
     (datum->syntax #f
       `(try ,(compile-statement tb)
             ,@(if cid `(#:catch ,cid ,(compile-statement cb)) '())
             ,@(if fb `(#:finally ,(compile-statement fb)) '()))
       loc)]))

(define (compile-variable-declaration stx)
  (match stx
    [(ecma:decl:var loc id expr)
     (let ([ret (datum-intern-literal
                 (symbol->string id))])
       (datum->syntax #f
         (if expr
             `(\, (= (id ,id) ,(compile-expression expr))
                  ,ret)
             ret)
         loc))]))

(define (compile-function stx)
  (match stx
    [(ecma:fn loc _ params body)
     (datum->syntax #f
       `(function ,params
          (declare-vars ,(extract-vars body))
          ,@(map compile-statement body)))]))

(define (compile-program prog)
  (datum->syntax #f
    `((declare-vars ,(extract-vars prog))
      ,@(map
         (λ (fn)
           `(declare-fn
             ,(ecma:fn-name fn)
             ,(compile-function fn)))
         (extract-functions prog))
      ,@(filter-map
         (λ (elt)
           (and (ecma:stmt? elt)
                (compile-statement elt)))
         prog))))
